module Main exposing (main)

import Angle
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color exposing (Color)
import Direction3d
import Html exposing (Html)
import Html.Attributes as HA
import Illuminance
import Keyboard
import Keyboard.Arrows
import Length
import List.Extra as List
import LuminousFlux
import Pixels
import Point3d
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import Set
import Task
import TriangularMesh
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Texture


type Msg
    = Tick Float
    | GotViewport Int Int
    | Resized
    | KeyPress Keyboard.Msg
    | GotTexture TextureId (Result WebGL.Texture.Error (Material.Texture Color))


type TextureId
    = GrassTx
    | WaterTx


type alias Texture =
    Material.Texture Color


type WorldCoordinates
    = WorldCoordinates


type alias Entity =
    Scene3d.Entity WorldCoordinates


type alias Model =
    { time : Float
    , deltas : List Float
    , width : Int
    , height : Int
    , pressedKeys : List Keyboard.Key
    , keyChange : Maybe Keyboard.KeyChange
    , player : Player
    , textures : Textures
    , floor : Maybe Entity
    , npcs : List Npc
    , dialog : Maybe Dialog
    }


type alias Dialog =
    { text : String
    , duration : Float
    , queue : List String
    }


type alias Player =
    { entity : Entity
    , pos : Vector3d Length.Meters WorldCoordinates
    }


type alias Npc =
    { entity : Entity
    , pos : Vector3d Length.Meters WorldCoordinates
    , dialog : List String
    }


type alias Textures =
    { grass : Maybe (Material.Texture Color)
    , water : Maybe (Material.Texture Color)
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        model : Model
        model =
            { time = 0
            , deltas = []
            , width = 0
            , height = 0
            , pressedKeys = []
            , keyChange = Nothing
            , player = player
            , textures = textures
            , floor = Nothing
            , npcs = npcs
            , dialog = Nothing
            }

        player =
            { entity = makeCube Color.lightBlue
            , pos = Vector3d.meters 5 11 0
            }

        textures =
            { grass = Nothing
            , water = Nothing
            }

        npcs =
            [ { entity = makeCube Color.darkRed
              , pos = Vector3d.meters 4 12 0
              , dialog =
                    [ "Hey"
                    , "What's up?"
                    ]
              }
            , { entity = makeCube Color.purple
              , pos = Vector3d.meters 14 6 0
              , dialog =
                    [ "Sup"
                    ]
              }
            ]

        cmd : Cmd Msg
        cmd =
            Cmd.batch
                [ getViewport
                , Task.attempt
                    (GotTexture GrassTx)
                    (Material.loadWith Material.nearestNeighborFiltering "/grass.png")
                , Task.attempt
                    (GotTexture WaterTx)
                    (Material.loadWith Material.nearestNeighborFiltering "/water.png")
                ]
    in
    ( model, cmd )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


getViewport : Cmd Msg
getViewport =
    Task.perform
        (\{ viewport } -> GotViewport (ceiling viewport.width) (ceiling viewport.height))
        Browser.Dom.getViewport



-- UPDATE


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Sub.map KeyPress Keyboard.subscriptions
        , Browser.Events.onResize (\_ _ -> Resized)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick d ->
            { model | time = model.time + d }
                |> gameTick d

        Resized ->
            ( model, getViewport )

        GotViewport width height ->
            let
                newModel =
                    { model
                        | width = width
                        , height = height
                    }
            in
            ( newModel, Cmd.none )

        KeyPress kMsg ->
            let
                ( pressedKeys, keyChange ) =
                    Keyboard.updateWithKeyChange Keyboard.anyKeyOriginal kMsg model.pressedKeys

                newModel =
                    { model
                        | pressedKeys = pressedKeys
                        , keyChange = keyChange
                    }
            in
            newModel |> keyEvent

        GotTexture key texture ->
            case texture of
                Ok tx ->
                    let
                        textures =
                            model.textures

                        newTextures =
                            case key of
                                GrassTx ->
                                    { textures | grass = Just tx }

                                WaterTx ->
                                    { textures | water = Just tx }

                        newModel =
                            { model | textures = newTextures }
                    in
                    ( newModel |> updateFloor, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateDeltas : Float -> Model -> Model
updateDeltas delta model =
    let
        oldDeltas =
            case model.deltas of
                [] ->
                    []

                _ :: rest ->
                    if List.length model.deltas > 120 then
                        rest

                    else
                        model.deltas

        newDeltas =
            oldDeltas ++ [ delta ]
    in
    { model | deltas = newDeltas }



-- update floor checks if all necessary textures are ready and creates the floor


updateFloor : Model -> Model
updateFloor model =
    case ( model.floor, model.textures.grass, model.textures.water ) of
        ( Nothing, Just grassTx, Just waterTx ) ->
            { model | floor = Just <| getFloor grassTx waterTx }

        _ ->
            model



-- units per ms


playerSpeed : Float
playerSpeed =
    1 / 1000


gameTick : Float -> Model -> ( Model, Cmd Msg )
gameTick d model =
    let
        arrows =
            Keyboard.Arrows.arrows model.pressedKeys

        player =
            model.player

        newPlayerPos =
            Vector3d.plus player.pos <|
                Vector3d.meters
                    (toFloat arrows.x * d * playerSpeed)
                    (toFloat arrows.y * d * playerSpeed)
                    0

        newPlayer =
            { player | pos = newPlayerPos }

        newDialog =
            Maybe.map (\dialog -> { dialog | duration = dialog.duration + d }) model.dialog

        newModel =
            { model
                | player = newPlayer
                , dialog = newDialog
            }
    in
    ( newModel |> updateDeltas d, Cmd.none )


keyEvent : Model -> ( Model, Cmd Msg )
keyEvent model =
    let
        newDialog =
            if wasKeyPressed Keyboard.Spacebar model then
                case model.dialog of
                    Nothing ->
                        findNewDialog model

                    Just d ->
                        advanceDialog d

            else
                model.dialog

        newModel =
            { model
                | dialog = newDialog
            }
    in
    ( newModel, Cmd.none )


findNewDialog : Model -> Maybe Dialog
findNewDialog model =
    findNpcToTalkWith model.player model.npcs
        |> Maybe.andThen (.dialog >> createDialog)


advanceDialog : Dialog -> Maybe Dialog
advanceDialog dialog =
    let
        timeToShowAllText =
            ((String.length dialog.text + 1) * 1000 // dialogCharactersPerSecond)
                |> toFloat
    in
    if dialog.duration > timeToShowAllText then
        createDialog dialog.queue

    else
        Just { dialog | duration = timeToShowAllText }


createDialog : List String -> Maybe Dialog
createDialog texts =
    case texts of
        [] ->
            Nothing

        text :: queue ->
            Just
                { text = text
                , duration = 0
                , queue = queue
                }


findNpcToTalkWith : Player -> List Npc -> Maybe Npc
findNpcToTalkWith player npcs =
    List.find (\npc -> isNearby player.pos npc.pos) npcs


isNearby :
    Vector3d Length.Meters WorldCoordinates
    -> Vector3d Length.Meters WorldCoordinates
    -> Bool
isNearby a b =
    let
        minDistance =
            2

        ( ax, ay, _ ) =
            Vector3d.toTuple Length.inMeters a

        ( bx, by, _ ) =
            Vector3d.toTuple Length.inMeters b
    in
    (ax - bx) ^ 2 + (ay - by) ^ 2 < (minDistance ^ 2)


wasKeyPressed : Keyboard.Key -> Model -> Bool
wasKeyPressed key model =
    model.keyChange == Just (Keyboard.KeyDown key)


makeCube : Color -> Entity
makeCube color =
    let
        -- 1x1m cube
        negative =
            Length.meters -0.5

        positive =
            Length.meters 0.5

        -- Define the eight vertices of the cube
        p1 =
            Point3d.xyz negative negative negative

        p2 =
            Point3d.xyz positive negative negative

        p3 =
            Point3d.xyz positive positive negative

        p4 =
            Point3d.xyz negative positive negative

        p5 =
            Point3d.xyz negative negative positive

        p6 =
            Point3d.xyz positive negative positive

        p7 =
            Point3d.xyz positive positive positive

        p8 =
            Point3d.xyz negative positive positive

        material =
            Material.matte color

        quad =
            Scene3d.quadWithShadow material

        bottom =
            quad p1 p2 p3 p4

        top =
            quad p5 p6 p7 p8

        front =
            quad p2 p3 p7 p6

        back =
            quad p1 p4 p8 p5

        left =
            quad p1 p2 p6 p5

        right =
            quad p4 p3 p7 p8
    in
    -- Combine all faces into a single entity
    Scene3d.group [ bottom, top, front, back, left, right ]
        |> Scene3d.translateBy (Vector3d.meters 0 0 0.5)



-- VIEW


view : Model -> Html msg
view model =
    case model.floor of
        Just floor ->
            gameView model floor

        _ ->
            Html.p [] [ Html.text "Loading" ]


gameView :
    Model
    -> Entity
    -> Html msg
gameView model floor =
    let
        player =
            model.player.entity |> Scene3d.translateBy model.player.pos

        npcs =
            List.map (\n -> Scene3d.translateBy n.pos n.entity) model.npcs

        cameraPos =
            Point3d.translateBy model.player.pos Point3d.origin

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = cameraPos
                        , azimuth = Angle.degrees -90
                        , elevation = Angle.degrees 30
                        , distance = Length.meters 10
                        }
                , verticalFieldOfView = Angle.degrees 45
                }
    in
    -- Render a scene with custom lighting and other settings
    Html.div []
        [ fpsView model.deltas
        , dialogView model.dialog
        , Scene3d.custom
            { entities = [ player, floor ] ++ npcs
            , camera = camera
            , background = Scene3d.backgroundColor Color.black
            , clipDepth = Length.meters 0.01
            , dimensions = ( Pixels.int 480, Pixels.int 270 )
            , lights = getLights model.time
            , exposure = Scene3d.exposureValue 5
            , whiteBalance = Light.skylight
            , antialiasing = Scene3d.multisampling
            , toneMapping = Scene3d.noToneMapping
            }
        ]


fpsView : List Float -> Html msg
fpsView deltas =
    Html.p
        [ HA.style "position" "absolute"
        , HA.style "text-shadow" "0 0 1px black"
        ]
        [ fpsFromDeltas deltas
            |> round
            |> String.fromInt
            |> Html.text
        ]


dialogView : Maybe Dialog -> Html msg
dialogView maybeDialog =
    case maybeDialog of
        Nothing ->
            Html.div [] []

        Just dialog ->
            Html.p
                [ HA.style "position" "absolute"
                , HA.style "bottom" "5vmin"
                , HA.style "font-size" "3vmin"
                , HA.style "left" "50vmin"
                , HA.style "right" "50vmin"
                , HA.style "padding" "6vmin 6vmin"
                , HA.style "background-color" "rgba(0, 0, 0, 75%)"
                ]
                [ dialogTextView dialog ]


dialogTextView : Dialog -> Html msg
dialogTextView dialog =
    let
        numCharacters =
            round (dialog.duration * dialogCharactersPerSecond) // 1000

        visibleText =
            String.left numCharacters dialog.text

        hiddenText =
            String.dropLeft numCharacters dialog.text
    in
    Html.span []
        [ Html.span []
            [ Html.text visibleText ]
        , Html.span
            [ HA.style "opacity" "0" ]
            [ Html.text hiddenText ]
        ]


dialogCharactersPerSecond : number
dialogCharactersPerSecond =
    10


fpsFromDeltas : List Float -> Float
fpsFromDeltas deltas =
    case deltas of
        [] ->
            0

        _ ->
            1000 / (List.sum deltas / (List.length deltas |> toFloat))


getLights : Float -> Scene3d.Lights coordinates
getLights t =
    let
        sunT =
            t / 1000 / 60

        sunDistance =
            100

        sunX =
            -sunDistance

        sunY =
            sin -sunT * sunDistance

        sunZBase =
            -- this value ranges from 1 (noon) to -1 (midnight)
            cos sunT

        sunZ =
            sunZBase * sunDistance

        sunCoords =
            Point3d.meters sunX sunY sunZ

        moonCoords =
            Point3d.meters (sunX / 2) (sunY / 2) (-sunZ / 2)

        sunLumens =
            if sunZBase > 0 then
                sunZBase * sunZBase * 10000000

            else
                0

        moonLumens =
            if sunZBase < 0 then
                sqrt -sunZBase * 2000000

            else
                0

        sunOrMoon =
            if sunLumens > 0 then
                Light.point (Light.castsShadows True)
                    { position = sunCoords
                    , chromaticity = Light.sunlight
                    , intensity = LuminousFlux.lumens sunLumens
                    }

            else
                Light.point (Light.castsShadows True)
                    { position = moonCoords
                    , chromaticity = Light.color Color.lightBlue
                    , intensity = LuminousFlux.lumens moonLumens
                    }

        softLightLux =
            let
                scale =
                    if sunZBase > 0 then
                        sunZBase * sunZBase

                    else
                        sunZBase / 10
            in
            scale * 35 + 15

        softLighting =
            Light.overhead
                { upDirection = Direction3d.z
                , chromaticity = Light.skylight
                , intensity = Illuminance.lux softLightLux
                }
    in
    Scene3d.twoLights sunOrMoon softLighting


getFloor :
    Texture
    -> Texture
    -> Entity
getFloor grassTx waterTx =
    let
        ( g, w ) =
            ( 0, 1 )

        textureFromId id =
            if id == g then
                grassTx

            else if id == w then
                waterTx

            else
                grassTx

        map =
            [ [ g, g, g, g, g, g, g, g, g, g, g, g, w, w, w, g ]
            , [ g, g, g, g, g, g, g, g, g, g, w, w, w, w, w, w ]
            , [ g, g, g, g, g, g, g, g, w, w, w, g, g, g, g, g ]
            , [ g, g, g, g, g, w, w, w, w, g, g, g, g, g, g, g ]
            , [ g, g, g, w, w, w, w, g, g, g, g, g, g, g, g, g ]
            , [ g, g, g, g, g, g, w, w, w, g, g, g, g, g, g, g ]
            , [ g, g, g, g, g, g, g, w, w, w, w, g, g, g, g, g ]
            , [ g, g, g, g, g, g, g, g, w, w, w, w, w, g, g, g ]
            , [ g, g, g, g, g, g, g, g, g, g, w, w, w, w, g, g ]
            , [ g, g, g, g, g, g, g, g, g, g, g, w, w, w, g, g ]
            , [ g, g, g, g, g, g, g, g, g, g, g, g, w, w, w, g ]
            , [ g, g, g, g, g, g, g, g, g, g, g, g, g, w, w, g ]
            , [ g, g, g, g, g, g, g, g, g, g, g, g, g, w, w, w ]
            , [ g, g, g, g, g, g, g, g, g, g, g, g, g, g, w, w ]
            , [ g, g, g, g, g, g, g, g, g, g, g, g, g, w, w, w ]
            , [ g, g, g, g, g, g, g, g, g, g, g, g, w, w, w, w ]
            ]

        mapsForTextures =
            getMapsForTextures map

        texturedTiles =
            List.map (mapAndTextureToEntity textureFromId) mapsForTextures
    in
    Scene3d.group texturedTiles


getMapsForTextures : List (List Int) -> List ( Int, List (List Bool) )
getMapsForTextures map =
    let
        allTextures =
            map
                |> List.concat
                |> Set.fromList
                |> Set.toList
    in
    List.map (getMapForTexture map) allTextures


getMapForTexture : List (List Int) -> Int -> ( Int, List (List Bool) )
getMapForTexture map id =
    let
        processRow =
            List.map ((==) id)
    in
    ( id, List.map processRow map )


mapAndTextureToEntity : (Int -> Texture) -> ( Int, List (List Bool) ) -> Entity
mapAndTextureToEntity textureFromId ( id, map ) =
    let
        tx =
            textureFromId id

        coords2dCell y x paint =
            if paint then
                Just ( toFloat x, toFloat y )

            else
                Nothing

        coords2dRow y =
            List.indexedMap (coords2dCell y)

        z =
            if id == 0 then
                0

            else
                -0.05

        toTexturedFacets ( x, y ) =
            [ ( { position = Point3d.meters x y z, uv = ( 0, 0 ) }
              , { position = Point3d.meters (x + 1) y z, uv = ( 1, 0 ) }
              , { position = Point3d.meters x (y + 1) z, uv = ( 0, 1 ) }
              )
            , ( { position = Point3d.meters x (y + 1) z, uv = ( 0, 1 ) }
              , { position = Point3d.meters (x + 1) (y + 1) z, uv = ( 1, 1 ) }
              , { position = Point3d.meters (x + 1) y z, uv = ( 1, 0 ) }
              )
            ]

        mesh =
            map
                |> List.indexedMap coords2dRow
                |> List.concat
                |> List.filterMap (Maybe.map toTexturedFacets)
                |> List.concat
                |> TriangularMesh.triangles
                |> Mesh.texturedFacets

        material =
            Material.texturedMatte tx
    in
    Scene3d.mesh material mesh
