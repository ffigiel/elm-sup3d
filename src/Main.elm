module Main exposing (main)

import Angle exposing (Angle)
import Axis3d
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


type alias Position =
    Vector3d Length.Meters ()


type alias Shape =
    Scene3d.Entity ()


type alias Model =
    { deltas : List Float
    , width : Int
    , height : Int
    , pressedKeys : List Keyboard.Key
    , keyChange : Maybe Keyboard.KeyChange
    , loadingErrors : List String
    , textures : Textures
    , floor : Maybe Shape
    , dialog : Maybe Dialog
    , world : World
    }


type alias Dialog =
    { title : String
    , text : String
    , duration : Float
    , queue : List String
    , talkingNpcId : EntityID
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
            { deltas = []
            , width = 0
            , height = 0
            , pressedKeys = []
            , keyChange = Nothing
            , loadingErrors = []
            , textures = textures
            , floor = Nothing
            , dialog = Nothing
            , world = initWorld
            }

        textures =
            { grass = Nothing
            , water = Nothing
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
    ( model, cmd )


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
            model
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

        GotNpcAction npcId ( action, duration ) ->
            let
                world =
                    model.world

                newWorld =
                    applyNpcAction action (world.time + duration) npcId model.world
            in
            ( { model | world = newWorld }, Cmd.none )


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
                |> timeSystem d
                |> playerMovementSystem d model.pressedKeys model.dialog
                |> npcActionSystem d
                |> smoothTurnSystem d
    in
    { model | world = newWorld }


timeSystem : Float -> World -> World
timeSystem d w =
    { w | time = w.time + d }


playerMovementSystem : Float -> List Keyboard.Key -> Maybe Dialog -> World -> World
playerMovementSystem d pressedKeys dialog w =
    case dialog of
        Just _ ->
            w

        _ ->
            let
                playerSpeed =
                    1.5

                arrows =
                    Keyboard.Arrows.arrows pressedKeys

                zeroVector =
                    Vector3d.meters 0 0 0

                dPos =
                    Vector3d.meters
                        (toFloat arrows.x * d * playerSpeed)
                        (toFloat arrows.y * d * playerSpeed)
                        0
            in
            if dPos == zeroVector then
                w

            else
                let
                    newPositions =
                        Component.update w.playerId (Vector3d.plus dPos) w.positions

                    newTargetAngle =
                        angleFromPoints zeroVector dPos

                    newAngles =
                        Component.update w.playerId (\( a, _ ) -> ( a, newTargetAngle )) w.angles
                in
                { w | positions = newPositions, angles = newAngles }


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


smoothTurnSystem : Float -> World -> World
smoothTurnSystem d w =
    System.step
        (\( angle, targetAngle ) ->
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
                    min 1 (d * 3)

                newAngle =
                    (degrees + (turnSpeed * normalizedDeltaDegrees))
                        |> Angle.degrees
            in
            ( newAngle, targetAngle )
        )
        angleSpec
        w


gameTick : Float -> Model -> ( Model, Cmd Msg )
gameTick d model =
    let
        newDialog =
            Maybe.map (\dialog -> { dialog | duration = dialog.duration + d }) model.dialog

        newModel =
            { model
                | dialog = newDialog
            }

        cmd =
            npcBehaviorCmd model.world
    in
    ( newModel |> updateDeltas d, cmd )


npcBehaviorCmd : World -> Cmd Msg
npcBehaviorCmd w =
    System.indexedFoldl
        (\npcId ( action, until ) acc ->
            case action of
                NpcTalking _ _ ->
                    acc

                _ ->
                    if until < w.time then
                        prepareNewNpcAction npcId :: acc

                    else
                        acc
        )
        w.npcActions
        []
        |> Cmd.batch


prepareNewNpcAction : EntityID -> Cmd Msg
prepareNewNpcAction npcId =
    Random.generate (GotNpcAction npcId) genNpcAction


genNpcAction : Random.Generator ( NpcAction, Float )
genNpcAction =
    Random.weighted
        ( 2, genNpcWaiting )
        [ ( 3, genNpcPacing )
        ]
        |> Random.andThen identity


genNpcWaiting : Random.Generator ( NpcAction, Float )
genNpcWaiting =
    Random.map
        (\duration -> ( NpcWaiting, duration ))
        (Random.float 1 4)


genNpcPacing : Random.Generator ( NpcAction, Float )
genNpcPacing =
    Random.map2 (\angle duration -> ( NpcPacing angle, duration ))
        (Random.float -60 60 |> Random.map Angle.degrees)
        (Random.float 1 3)



-- EVENTS


keyEvent : Model -> ( Model, Cmd Msg )
keyEvent model =
    let
        ( newDialog, newWorld ) =
            if wasKeyPressed Keyboard.Spacebar model then
                case model.dialog of
                    Nothing ->
                        findNewDialog model.world

                    Just d ->
                        advanceDialog d model.world

            else
                ( model.dialog, model.world )

        newModel =
            { model
                | dialog = newDialog
                , world = newWorld
            }
    in
    ( newModel, Cmd.none )


findNewDialog : World -> ( Maybe Dialog, World )
findNewDialog world =
    case Component.get world.playerId world.positions of
        Nothing ->
            ( Nothing, world )

        Just playerPos ->
            case findNpcToTalkWith playerPos world of
                Just npcId ->
                    let
                        maybeNpc =
                            Maybe.map4
                                (\pos dialogTexts name ( npcAction, npcActionUntil ) ->
                                    { pos = pos
                                    , dialogTexts = dialogTexts
                                    , name = name
                                    , npcAction = npcAction
                                    , npcActionUntil = npcActionUntil
                                    }
                                )
                                (Component.get npcId world.positions)
                                (Component.get npcId world.dialogs)
                                (Component.get npcId world.names)
                                (Component.get npcId world.npcActions)
                    in
                    case maybeNpc of
                        Just { pos, dialogTexts, name, npcAction, npcActionUntil } ->
                            let
                                newAngle =
                                    angleFromPoints pos playerPos

                                newNpcAction =
                                    NpcTalking newAngle ( npcAction, npcActionUntil )

                                newWorld =
                                    applyNpcAction newNpcAction 0 npcId world

                                dialog =
                                    createDialog
                                        { title = name
                                        , texts = dialogTexts
                                        , talkingNpcId = npcId
                                        }
                            in
                            ( dialog, newWorld )

                        _ ->
                            ( Nothing, world )

                Nothing ->
                    ( Nothing, world )


applyNpcAction : NpcAction -> Float -> EntityID -> World -> World
applyNpcAction action until npcId w =
    case Component.get npcId w.angles of
        Nothing ->
            w

        Just ( angle, targetAngle ) ->
            let
                newTargetAngle =
                    case action of
                        NpcPacing a ->
                            a

                        NpcTalking a _ ->
                            a

                        _ ->
                            targetAngle

                newAngles =
                    Component.set npcId ( angle, newTargetAngle ) w.angles

                newNpcActions =
                    Component.set npcId ( action, until ) w.npcActions
            in
            { w
                | angles = newAngles
                , npcActions = newNpcActions
            }


angleFromPoints : Position -> Position -> Angle
angleFromPoints a b =
    let
        diff =
            Vector3d.minus a b
    in
    Angle.atan2 (Vector3d.yComponent diff) (Vector3d.xComponent diff)


advanceDialog : Dialog -> World -> ( Maybe Dialog, World )
advanceDialog dialog world =
    let
        minAdvanceDuration =
            minDurationToAdvanceDialogText dialog.text
    in
    if dialog.duration > minAdvanceDuration then
        if dialog.queue == [] then
            ( Nothing, stopNpcTalking dialog.talkingNpcId world )

        else
            ( createDialog
                { title = dialog.title
                , texts = dialog.queue
                , talkingNpcId = dialog.talkingNpcId
                }
            , world
            )

    else
        ( Just { dialog | duration = minAdvanceDuration }, world )


stopNpcTalking : EntityID -> World -> World
stopNpcTalking npcId world =
    let
        newNpcActions =
            Component.update npcId
                (\( action, until ) ->
                    case action of
                        NpcTalking _ ( prevAction, prevUntil ) ->
                            ( prevAction, prevUntil )

                        _ ->
                            ( action, until )
                )
                world.npcActions
    in
    { world | npcActions = newNpcActions }


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


findNpcToTalkWith : Position -> World -> Maybe EntityID
findNpcToTalkWith playerPos world =
    System.indexedFoldl2
        (\npcId _ npcPos result ->
            case result of
                Just _ ->
                    result

                Nothing ->
                    if isNearby playerPos npcPos then
                        Just npcId

                    else
                        Nothing
        )
        world.npcActions
        world.positions
        Nothing


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
        shapes =
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

        playerPos =
            Component.get model.world.playerId model.world.positions
                |> Maybe.withDefault (Vector3d.meters 0 0 0)

        cameraPos =
            Point3d.translateBy playerPos Point3d.origin

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
            { entities = floor :: shapes
            , camera = camera
            , background = Scene3d.backgroundColor Color.black
            , clipDepth = Length.meters 0.01
            , dimensions = ( Pixels.int 800, Pixels.int 480 )
            , lights = getLights model.world.time
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
    { time : Float -- in seconds
    , shapes : Component.Set Shape
    , positions : Component.Set Position
    , angles : Component.Set ( Angle, Angle )
    , names : Component.Set String
    , dialogs : Component.Set (List String)
    , npcActions : Component.Set ( NpcAction, Float )
    , playerId : EntityID
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
            { time = 0
            , shapes = Component.empty
            , positions = Component.empty
            , angles = Component.empty
            , names = Component.empty
            , dialogs = Component.empty
            , npcActions = Component.empty
            , playerId = -1
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

        playerEntity : ( EntityID, World ) -> ( EntityID, World )
        playerEntity ( i, w ) =
            Entity.create (i + 1) { w | playerId = i + 1 }
                |> Entity.with ( shapeSpec, makeCube Color.lightBlue )
                |> Entity.with ( positionSpec, Vector3d.meters 8 8 0 )
                |> Entity.with ( angleSpec, ( initAngle, initAngle ) )
    in
    ( 0, world )
        |> (\a -> List.foldl npcEntity a npcData)
        |> playerEntity
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
