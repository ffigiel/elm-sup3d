module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import Task
import WebGL exposing (Mesh, Shader)


type alias Model =
    { time : Float
    , width : Int
    , height : Int
    }


type Msg
    = Tick Float
    | GotViewport Browser.Dom.Viewport


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { time = 0
            , width = 0
            , height = 0
            }

        cmd =
            Task.perform GotViewport Browser.Dom.getViewport
    in
    ( model, cmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick d ->
            ( { model | time = model.time + d }, Cmd.none )

        GotViewport { viewport } ->
            let
                newModel =
                    { model
                        | width = floor viewport.width
                        , height = floor viewport.height
                    }
            in
            ( newModel, Cmd.none )


view : Model -> Html msg
view model =
    if ( model.width, model.height ) == ( 0, 0 ) then
        Html.div [] [ Html.text "Loading..." ]

    else
        WebGL.toHtml
            [ HA.width model.width
            , HA.height model.height
            , HA.style "display" "block"
            ]
            [ WebGL.entity
                vertexShader
                fragmentShader
                mesh
                { perspective = perspective (model.time / 10000) }
            ]


perspective : Float -> Mat4
perspective t =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
