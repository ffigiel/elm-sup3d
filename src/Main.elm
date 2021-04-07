module Main exposing (main)

import Angle exposing (Angle)
import Axis3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color exposing (Color)
import Dict exposing (Dict)
import Direction3d
import Html exposing (Html)
import Html.Attributes as HA
import Illuminance
import Keyboard
import Keyboard.Arrows
import Length
import List.Extra as List
import Logic.Component as Component
import Logic.Entity as Entity exposing (EntityID)
import Logic.System as System
import LuminousFlux
import Pixels
import Point3d
import Random
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import Set
import SketchPlane3d
import Task
import TriangularMesh
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Texture



-- TYPES


type Msg
    = Tick Float
    | GotViewport Int Int
    | Resized
    | KeyPress Keyboard.Msg
    | GotTexture TextureId (Result WebGL.Texture.Error (Material.Texture Color))
    | GotNpcAction Int ( NpcAction, Float )


type TextureId
    = GrassTx
    | WaterTx


type alias Texture =
    Material.Texture Color


type SceneCoordinates
    = SceneCoordinates


type alias Position =
    Vector3d Length.Meters SceneCoordinates


type alias Shape =
    Scene3d.Entity SceneCoordinates


type alias Model =
    { time : Float -- in seconds
    , deltas : List Float
    , width : Int
    , height : Int
    , pressedKeys : List Keyboard.Key
    , keyChange : Maybe Keyboard.KeyChange
    , player : Player
    , loadingErrors : List String
    , textures : Textures
    , floor : Maybe Shape
    , nextNpcId : Int
    , npcs : Dict Int Npc
    , dialog : Maybe Dialog
    , world : World
    }


type alias Dialog =
    { title : String
    , text : String
    , duration : Float
    , queue : List String
    , talkingNpcId : Int
    }


type alias Player =
    { entity : Shape
    , pos : Position
    , angle : Angle
    , targetAngle : Angle
    }


type alias Npc =
    { id : Int
    , name : String
    , entity : Shape
    , pos : Position
    , angle : Angle
    , targetAngle : Angle
    , dialog : List String
    , action : NpcAction
    , actionTimeLeft : Float
    }


type NpcAction
    = NpcWaiting
    | NpcPacing Angle
    | NpcTalking Angle ( NpcAction, Float )


type alias Textures =
    { grass : Maybe (Material.Texture Color)
    , water : Maybe (Material.Texture Color)
    }


type alias List2d a =
    List (List a)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- INIT


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
            , loadingErrors = []
            , textures = textures
            , floor = Nothing
            , nextNpcId = 0
            , npcs = Dict.empty
            , dialog = Nothing
            , world = initWorld
            }

        player =
            { entity = makeCube Color.lightBlue
            , angle = Angle.degrees 0
            , targetAngle = Angle.degrees 0
            , pos = Vector3d.meters 8 8 0
            }

        textures =
            { grass = Nothing
            , water = Nothing
            }

        npc1 =
            { name = "Viola"
            , color = Color.purple
            , pos = Vector3d.meters 4 12 0
            , dialog =
                [ "Hey!"
                , "What's up?"
                ]
            }

        npc2 =
            { name = "Redd"
            , color = Color.darkRed
            , pos = Vector3d.meters 14 6 0
            , dialog =
                [ "Sup."
                ]
            }

        textureCmds =
            [ GrassTx, WaterTx ]
                |> List.map
                    (\id ->
                        Task.attempt
                            (GotTexture id)
                            (Material.loadWith Material.nearestNeighborFiltering (getTextureUrl id))
                    )

        cmd : Cmd Msg
        cmd =
            Cmd.batch
                (getViewport :: textureCmds)
    in
    ( model
        |> addNpc npc1
        |> addNpc npc2
    , cmd
    )


addNpc :
    { name : String
    , color : Color
    , pos : Position
    , dialog : List String
    }
    -> Model
    -> Model
addNpc { name, color, pos, dialog } model =
    let
        npc =
            { id = model.nextNpcId
            , name = name
            , entity = makeCube color
            , pos = pos
            , dialog = dialog
            , angle = Angle.degrees 90
            , targetAngle = Angle.degrees 90
            , action = NpcWaiting
            , actionTimeLeft = 0
            }
    in
    { model
        | nextNpcId = model.nextNpcId + 1
        , npcs = Dict.insert npc.id npc model.npcs
    }


getViewport : Cmd Msg
getViewport =
    Task.perform
        (\{ viewport } -> GotViewport (ceiling viewport.width) (ceiling viewport.height))
        Browser.Dom.getViewport


getTextureUrl : TextureId -> String
getTextureUrl id =
    case id of
        GrassTx ->
            "/grass.png"

        WaterTx ->
            "/water.png"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (\d -> Tick (d / 1000))
        , Sub.map KeyPress Keyboard.subscriptions
        , Browser.Events.onResize (\_ _ -> Resized)
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick d ->
            { model | time = model.time + d }
                |> ecsTick d
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

        GotTexture txId texture ->
            case texture of
                Ok tx ->
                    let
                        textures =
                            model.textures

                        newTextures =
                            case txId of
                                GrassTx ->
                                    { textures | grass = Just tx }

                                WaterTx ->
                                    { textures | water = Just tx }

                        newModel =
                            { model | textures = newTextures }
                    in
                    ( newModel |> updateFloor, Cmd.none )

                Err err ->
                    let
                        newError =
                            case err of
                                WebGL.Texture.LoadError ->
                                    "Could not load texture "
                                        ++ getTextureUrl txId

                                WebGL.Texture.SizeError x y ->
                                    "Texture "
                                        ++ getTextureUrl txId
                                        ++ " has invalid dimensions ("
                                        ++ String.fromInt x
                                        ++ "x"
                                        ++ String.fromInt y
                                        ++ ")"
                    in
                    ( { model | loadingErrors = model.loadingErrors ++ [ newError ] }, Cmd.none )

        GotNpcAction npcId ( action, actionTimeLeft ) ->
            let
                newNpcs =
                    Dict.update
                        npcId
                        (Maybe.map <| applyNpcAction action actionTimeLeft)
                        model.npcs
            in
            ( { model | npcs = newNpcs }, Cmd.none )


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


updateFloor : Model -> Model
updateFloor model =
    -- updateFloor checks if all necessary textures are ready and creates the floor
    case ( model.floor, model.textures.grass, model.textures.water ) of
        ( Nothing, Just grassTx, Just waterTx ) ->
            { model | floor = Just <| makeFloor grassTx waterTx }

        _ ->
            model



-- TICK


ecsTick : Float -> Model -> Model
ecsTick d model =
    let
        newWorld =
            model.world
                |> npcActionSystem d
    in
    { model | world = newWorld }


npcActionSystem : Float -> World -> World
npcActionSystem d w =
    System.step3
        (\( ( action, _ ), _ ) ( pos, setPos ) ( ( angle, _ ), _ ) acc ->
            case action of
                NpcPacing _ ->
                    let
                        npcSpeed =
                            0.5

                        dPos =
                            Vector3d.rThetaOn
                                SketchPlane3d.xy
                                (Length.meters <| d * npcSpeed)
                                angle

                        newPos =
                            Vector3d.plus
                                pos
                                dPos
                    in
                    acc
                        |> setPos newPos

                _ ->
                    acc
        )
        npcActionSpec
        positionSpec
        angleSpec
        w


gameTick : Float -> Model -> ( Model, Cmd Msg )
gameTick d model =
    let
        playerSpeed =
            1.5

        arrows =
            Keyboard.Arrows.arrows model.pressedKeys

        player =
            model.player

        newPlayerPos =
            case model.dialog of
                Nothing ->
                    Vector3d.plus player.pos <|
                        Vector3d.meters
                            (toFloat arrows.x * d * playerSpeed)
                            (toFloat arrows.y * d * playerSpeed)
                            0

                _ ->
                    player.pos

        newPlayerTargetAngle =
            if model.player.pos == newPlayerPos then
                model.player.targetAngle

            else
                angleFromPoints model.player.pos newPlayerPos

        newPlayer =
            { player
                | pos = newPlayerPos
                , angle = angleTick d ( model.player.angle, newPlayerTargetAngle )
                , targetAngle = newPlayerTargetAngle
            }

        ( newNpcs, npcCmds ) =
            npcsTick d model.npcs

        newDialog =
            Maybe.map (\dialog -> { dialog | duration = dialog.duration + d }) model.dialog

        newModel =
            { model
                | player = newPlayer
                , npcs = newNpcs
                , dialog = newDialog
            }

        cmd =
            Cmd.batch npcCmds
    in
    ( newModel |> updateDeltas d, cmd )


npcsTick : Float -> Dict Int Npc -> ( Dict Int Npc, List (Cmd Msg) )
npcsTick d npcs =
    let
        ( npcValues, cmds ) =
            Dict.values npcs
                |> List.map (npcTick d)
                |> List.unzip

        newNpcs =
            npcValues
                |> List.map (\n -> ( n.id, n ))
                |> Dict.fromList
    in
    ( newNpcs, cmds )


npcTick : Float -> Npc -> ( Npc, Cmd Msg )
npcTick d npc =
    let
        newNpc =
            { npc
                | angle = angleTick d ( npc.angle, npc.targetAngle )
            }
    in
    case newNpc.action of
        NpcTalking _ _ ->
            ( newNpc, Cmd.none )

        _ ->
            npcTickAction d { newNpc | actionTimeLeft = newNpc.actionTimeLeft - d }


angleTick : Float -> ( Angle, Angle ) -> Angle
angleTick d ( angle, targetAngle ) =
    let
        degrees =
            Angle.inDegrees angle

        targetDegrees =
            Angle.inDegrees targetAngle

        deltaDegrees =
            targetDegrees - degrees

        normalizedDeltaDegrees =
            if deltaDegrees > 180 then
                deltaDegrees - 360

            else
                deltaDegrees

        turnSpeed =
            min 1 (d * 4)
    in
    (degrees + (turnSpeed * normalizedDeltaDegrees))
        |> Angle.degrees


npcTickAction : Float -> Npc -> ( Npc, Cmd Msg )
npcTickAction d npc =
    if npc.actionTimeLeft <= 0 then
        ( npc, prepareNewNpcAction npc )

    else
        ( npc, Cmd.none )


prepareNewNpcAction : Npc -> Cmd Msg
prepareNewNpcAction npc =
    Random.generate (GotNpcAction npc.id) (genNpcAction npc)


genNpcAction : Npc -> Random.Generator ( NpcAction, Float )
genNpcAction npc =
    Random.weighted
        ( 2, genNpcWaiting npc )
        [ ( 3, genNpcPacing npc )
        ]
        |> Random.andThen identity


genNpcWaiting : Npc -> Random.Generator ( NpcAction, Float )
genNpcWaiting _ =
    Random.map
        (\duration -> ( NpcWaiting, duration ))
        (Random.float 1 4)


genNpcPacing : Npc -> Random.Generator ( NpcAction, Float )
genNpcPacing npc =
    let
        npcDegrees =
            Angle.inDegrees npc.angle
    in
    Random.map2 (\angle duration -> ( NpcPacing angle, duration ))
        (Random.float (npcDegrees - 60) (npcDegrees + 60) |> Random.map Angle.degrees)
        (Random.float 1 3)



-- EVENTS


keyEvent : Model -> ( Model, Cmd Msg )
keyEvent model =
    let
        ( newDialog, newNpcs ) =
            if wasKeyPressed Keyboard.Spacebar model then
                case model.dialog of
                    Nothing ->
                        findNewDialog model

                    Just d ->
                        advanceDialog d model.npcs

            else
                ( model.dialog, model.npcs )

        newModel =
            { model
                | npcs = newNpcs
                , dialog = newDialog
            }
    in
    ( newModel, Cmd.none )


findNewDialog : Model -> ( Maybe Dialog, Dict Int Npc )
findNewDialog model =
    case findNpcToTalkWith model.player model.npcs of
        Just talkingNpc ->
            let
                newAngle =
                    angleFromPoints talkingNpc.pos model.player.pos

                newAction =
                    NpcTalking newAngle ( talkingNpc.action, talkingNpc.actionTimeLeft )

                newNpcs =
                    Dict.update
                        talkingNpc.id
                        (Maybe.map <| applyNpcAction newAction 0)
                        model.npcs

                dialog =
                    createDialog
                        { title = talkingNpc.name
                        , texts = talkingNpc.dialog
                        , talkingNpcId = talkingNpc.id
                        }
            in
            ( dialog, newNpcs )

        Nothing ->
            ( Nothing, model.npcs )


applyNpcAction : NpcAction -> Float -> Npc -> Npc
applyNpcAction action actionTimeLeft npc =
    let
        newTargetAngle =
            case action of
                NpcPacing angle ->
                    angle

                NpcTalking angle _ ->
                    angle

                _ ->
                    npc.targetAngle
    in
    { npc
        | action = action
        , actionTimeLeft = actionTimeLeft
        , targetAngle = newTargetAngle
    }


angleFromPoints : Position -> Position -> Angle
angleFromPoints a b =
    let
        diff =
            Vector3d.minus a b
    in
    Angle.atan2 (Vector3d.yComponent diff) (Vector3d.xComponent diff)


advanceDialog : Dialog -> Dict Int Npc -> ( Maybe Dialog, Dict Int Npc )
advanceDialog dialog npcs =
    let
        minAdvanceDuration =
            minDurationToAdvanceDialogText dialog.text
    in
    if dialog.duration > minAdvanceDuration then
        if dialog.queue == [] then
            ( Nothing, stopNpcTalking dialog.talkingNpcId npcs )

        else
            ( createDialog
                { title = dialog.title
                , texts = dialog.queue
                , talkingNpcId = dialog.talkingNpcId
                }
            , npcs
            )

    else
        ( Just { dialog | duration = minAdvanceDuration }, npcs )


stopNpcTalking : Int -> Dict Int Npc -> Dict Int Npc
stopNpcTalking npcId npcs =
    Dict.update
        npcId
        (Maybe.map
            (\npc ->
                case npc.action of
                    NpcTalking _ ( action, actionTimeLeft ) ->
                        applyNpcAction action actionTimeLeft npc

                    _ ->
                        npc
            )
        )
        npcs


minDurationToAdvanceDialogText : String -> Float
minDurationToAdvanceDialogText text =
    toFloat (String.length text + 1) / dialogCharactersPerSecond


createDialog :
    { title : String
    , texts : List String
    , talkingNpcId : Int
    }
    -> Maybe Dialog
createDialog { title, texts, talkingNpcId } =
    case texts of
        [] ->
            Nothing

        text :: queue ->
            Just
                { title = title
                , text = text
                , duration = 0
                , queue = queue
                , talkingNpcId = talkingNpcId
                }


findNpcToTalkWith : Player -> Dict Int Npc -> Maybe Npc
findNpcToTalkWith player npcs =
    npcs
        |> Dict.values
        |> List.find (\npc -> isNearby player.pos npc.pos)


isNearby : Position -> Position -> Bool
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



-- ENTITIES


makeCube : Color -> Shape
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

        side =
            Scene3d.quadWithShadow material

        bottom =
            side p1 p2 p3 p4

        top =
            side p5 p6 p7 p8

        front =
            side p2 p3 p7 p6

        back =
            side p1 p4 p8 p5

        left =
            side p1 p2 p6 p5

        right =
            side p4 p3 p7 p8

        eyeCenters =
            [ ( -0.2, 0.2 )
            , ( 0.2, 0.2 )
            ]

        eyeWhites =
            makeEyes Color.white 0.1 0.501

        eyePupils =
            makeEyes Color.black 0.05 0.502

        makeEyes eyeColor size distance =
            eyeCenters
                |> List.map
                    (\( x, z ) ->
                        let
                            x1 =
                                Length.meters (x - size)

                            x2 =
                                Length.meters (x + size)

                            y =
                                Length.meters distance

                            z1 =
                                Length.meters (z - size)

                            z2 =
                                Length.meters (z + size)
                        in
                        Scene3d.quad (Material.matte eyeColor)
                            (Point3d.xyz x1 y z1)
                            (Point3d.xyz x1 y z2)
                            (Point3d.xyz x2 y z2)
                            (Point3d.xyz x2 y z1)
                    )
                |> Scene3d.group
    in
    -- Combine all faces into a single entity
    Scene3d.group [ bottom, top, front, back, left, right, eyeWhites, eyePupils ]
        |> Scene3d.rotateAround Axis3d.z (Angle.degrees -90)
        |> Scene3d.translateBy (Vector3d.meters 0 0 0.5)


makeFloor :
    Texture
    -> Texture
    -> Shape
makeFloor grassTx waterTx =
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


getMapsForTextures : List2d Int -> List ( Int, List2d Bool )
getMapsForTextures map =
    let
        allTextures =
            map
                |> List.concat
                |> Set.fromList
                |> Set.toList
    in
    List.map (getMapForTexture map) allTextures


getMapForTexture : List2d Int -> Int -> ( Int, List2d Bool )
getMapForTexture map id =
    let
        processRow =
            List.map ((==) id)
    in
    ( id, List.map processRow map )


mapAndTextureToEntity : (Int -> Texture) -> ( Int, List2d Bool ) -> Shape
mapAndTextureToEntity textureFromId ( id, map ) =
    let
        tx =
            textureFromId id

        coords2dCell y x paint =
            if paint then
                Just ( toFloat x, toFloat y )

            else
                Nothing

        yMax =
            List.length map

        coords2dRow y =
            -- reverse the y coord to adjust it to world coords
            List.indexedMap (coords2dCell (yMax - y))

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



-- VIEW


view : Model -> Html msg
view model =
    case ( model.loadingErrors, model.floor ) of
        ( [], Just floor ) ->
            gameView model floor

        ( [], Nothing ) ->
            Html.div [] [ Html.text "Loading" ]

        _ ->
            model.loadingErrors
                |> List.map (Html.text >> List.singleton >> Html.p [])
                |> Html.div []


gameView : Model -> Shape -> Html msg
gameView model floor =
    let
        player =
            model.player.entity
                |> Scene3d.rotateAround Axis3d.z model.player.angle
                |> Scene3d.translateBy model.player.pos

        npcs =
            System.foldl3
                (\shape ( angle, _ ) position acc ->
                    (shape
                        |> Scene3d.rotateAround Axis3d.z angle
                        |> Scene3d.translateBy position
                    )
                        :: acc
                )
                (shapeSpec.get model.world)
                (angleSpec.get model.world)
                (positionSpec.get model.world)
                []

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
            , dimensions = ( Pixels.int 800, Pixels.int 480 )
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
        , HA.style "font-size" "2vmin"
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
            Html.div
                [ HA.style "position" "absolute"
                , HA.style "bottom" "5vmin"
                , HA.style "left" "5vmin"
                , HA.style "right" "5vmin"
                ]
                [ Html.div
                    [ HA.style "padding" "5vmin"
                    , HA.style "margin-left" "auto"
                    , HA.style "margin-right" "auto"
                    , HA.style "width" "80vmin"
                    , HA.style "background-color" "rgba(0, 0, 0, 75%)"
                    ]
                    [ dialogTitleView dialog
                    , dialogTextView dialog
                    ]
                ]


dialogTitleView : Dialog -> Html msg
dialogTitleView dialog =
    Html.p
        [ HA.style "font-weight" "bold"
        , HA.style "color" "#999"
        ]
        [ Html.text dialog.title ]


dialogTextView : Dialog -> Html msg
dialogTextView dialog =
    let
        numCharacters =
            floor (dialog.duration * dialogCharactersPerSecond)

        visibleText =
            String.left numCharacters dialog.text

        hiddenText =
            String.dropLeft numCharacters dialog.text
    in
    Html.p []
        [ Html.span []
            [ Html.text visibleText ]
        , Html.span
            [ HA.style "opacity" "0" ]
            [ Html.text hiddenText ]
        ]


dialogCharactersPerSecond : Float
dialogCharactersPerSecond =
    40


fpsFromDeltas : List Float -> Float
fpsFromDeltas deltas =
    case deltas of
        [] ->
            0

        _ ->
            1 / (List.sum deltas / (List.length deltas |> toFloat))


getLights : Float -> Scene3d.Lights coordinates
getLights t =
    let
        sunT =
            t / 60

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



-- ECS


type alias World =
    { shapes : Component.Set Shape
    , positions : Component.Set Position
    , angles : Component.Set ( Angle, Angle )
    , names : Component.Set String
    , dialogs : Component.Set (List String)
    , npcActions : Component.Set ( NpcAction, Float )
    }


type alias NpcData =
    { color : Color
    , pos : Position
    , name : String
    , dialog : List String
    }


initWorld : World
initWorld =
    let
        world =
            { shapes = Component.empty
            , positions = Component.empty
            , angles = Component.empty
            , names = Component.empty
            , dialogs = Component.empty
            , npcActions = Component.empty
            }

        npcData : List NpcData
        npcData =
            [ { color = Color.purple
              , pos = Vector3d.meters 4 12 0
              , name = "Viola"
              , dialog =
                    [ "Hey!"
                    , "What's up?"
                    ]
              }
            , { color = Color.darkRed
              , pos = Vector3d.meters 14 6 0
              , name = "Redd"
              , dialog =
                    [ "Sup."
                    ]
              }
            ]

        initAngle =
            Angle.degrees 90

        npcEntity : NpcData -> ( EntityID, World ) -> ( EntityID, World )
        npcEntity { color, pos, name, dialog } ( i, w ) =
            Entity.create (i + 1) w
                |> Entity.with ( shapeSpec, makeCube color )
                |> Entity.with ( positionSpec, pos )
                |> Entity.with ( angleSpec, ( initAngle, initAngle ) )
                |> Entity.with ( nameSpec, name )
                |> Entity.with ( dialogSpec, dialog )
                |> Entity.with ( npcActionSpec, ( NpcWaiting, 0 ) )
    in
    ( 0, world )
        |> (\a -> List.foldl npcEntity a npcData)
        |> Tuple.second


shapeSpec : Component.Spec Shape { w | shapes : Component.Set Shape }
shapeSpec =
    Component.Spec .shapes (\c w -> { w | shapes = c })


positionSpec : Component.Spec Position { w | positions : Component.Set Position }
positionSpec =
    Component.Spec .positions (\c w -> { w | positions = c })


angleSpec : Component.Spec ( Angle, Angle ) { w | angles : Component.Set ( Angle, Angle ) }
angleSpec =
    Component.Spec .angles (\c w -> { w | angles = c })


nameSpec : Component.Spec String { w | names : Component.Set String }
nameSpec =
    Component.Spec .names (\c w -> { w | names = c })


dialogSpec : Component.Spec (List String) { w | dialogs : Component.Set (List String) }
dialogSpec =
    Component.Spec .dialogs (\c w -> { w | dialogs = c })


npcActionSpec :
    Component.Spec
        ( NpcAction, Float )
        { w | npcActions : Component.Set ( NpcAction, Float ) }
npcActionSpec =
    Component.Spec .npcActions (\c w -> { w | npcActions = c })
