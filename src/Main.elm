module Main exposing (main)

import Angle
import Array
import Axis3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Html.Attributes as HA
import Illuminance
import Keyboard
import Keyboard.Arrows
import Length
import LuminousFlux
import Parameter1d
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import SketchPlane3d
import Sphere3d
import Task
import Temperature
import Triangle3d
import TriangularMesh
import Vector3d exposing (Vector3d)
import Viewpoint3d


type Msg
    = Tick Float
    | GotViewport Int Int
    | Resized
    | KeyPress Keyboard.Msg


type alias Model =
    { time : Float
    , width : Int
    , height : Int
    , keys : List Keyboard.Key
    , playerPos : Vector3d Length.Meters WorldCoordinates
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        model : Model
        model =
            { time = 0
            , width = 0
            , height = 0
            , keys = []
            , playerPos = Vector3d.meters 1 0 0
            }

        cmd : Cmd Msg
        cmd =
            Cmd.batch
                [ getViewport ]
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


unitSize : ( Float, Float )
unitSize =
    ( 1, 1 )


textures =
    { player = "player.png"
    , grass = "grass.png"
    , water = "water.png"
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
                |> tick d

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
                keys =
                    Keyboard.update kMsg model.keys
            in
            ( { model | keys = keys }, Cmd.none )



-- units per ms


playerSpeed : Float
playerSpeed =
    1 / 1000


tick : Float -> Model -> ( Model, Cmd Msg )
tick d model =
    let
        arrows =
            Keyboard.Arrows.arrows model.keys

        newPos =
            Vector3d.plus model.playerPos <|
                Vector3d.meters
                    (toFloat -arrows.y * d * playerSpeed)
                    (toFloat arrows.x * d * playerSpeed)
                    0
    in
    ( { model | playerPos = newPos }, Cmd.none )



-- VIEW


type WorldCoordinates
    = WorldCoordinates


cubeEntity : Scene3d.Entity WorldCoordinates
cubeEntity =
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
            Material.nonmetal
                { baseColor = Color.lightBlue
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                }

        quad =
            Scene3d.quadWithShadow material

        -- Create the six faces with different colors
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


view : Model -> Html msg
view model =
    let
        t =
            model.time / 100

        rotatedCube =
            cubeEntity |> Scene3d.translateBy model.playerPos

        floor =
            Scene3d.quad (Material.matte Color.darkGrey)
                (Point3d.meters 5 5 0)
                (Point3d.meters -5 5 0)
                (Point3d.meters -5 -5 0)
                (Point3d.meters 5 -5 0)

        cameraPos =
            Point3d.translateBy model.playerPos Point3d.origin

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbit
                        { focalPoint = cameraPos
                        , groundPlane = SketchPlane3d.xy
                        , azimuth = Angle.degrees 0
                        , elevation = Angle.degrees 30
                        , distance = Length.meters 10
                        }
                , verticalFieldOfView = Angle.degrees 45
                }
    in
    -- Render a scene with custom lighting and other settings
    Scene3d.custom
        { entities = [ rotatedCube, floor ]
        , camera = camera
        , background = Scene3d.backgroundColor Color.black
        , clipDepth = Length.meters 0.01
        , dimensions = ( Pixels.int model.width, Pixels.int model.height )
        , lights = getLights model.time
        , exposure = Scene3d.exposureValue 5
        , whiteBalance = Light.skylight
        , antialiasing = Scene3d.multisampling
        , toneMapping = Scene3d.noToneMapping
        }


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
