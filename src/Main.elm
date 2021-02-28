module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Html exposing (Html)
import Html.Attributes as HA
import Task


type alias Model =
    { time : Float
    , camera : Camera
    , width : Int
    , height : Int
    , resources : Resources
    }


type Msg
    = Tick Float
    | GotViewport Int Int
    | Resized
    | GotResources Resources.Msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


playerTextureUrl : String
playerTextureUrl =
    "player.png"


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        model : Model
        model =
            { time = 0
            , width = 0
            , height = 0
            , camera = Camera.fixedArea (2 * 2 * 16 * 9) ( 0, 0 )
            , resources = Resources.init
            }

        loadResources =
            Cmd.map GotResources (Resources.loadTextures [ playerTextureUrl ])

        cmd : Cmd Msg
        cmd =
            Cmd.batch
                [ loadResources
                , getViewport
                ]
    in
    ( model, cmd )


getViewport : Cmd Msg
getViewport =
    Task.perform
        (\{ viewport } -> GotViewport (ceiling viewport.width) (ceiling viewport.height))
        Browser.Dom.getViewport


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onResize (\_ _ -> Resized)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick d ->
            ( { model | time = model.time + d }, Cmd.none )

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

        GotResources rMsg ->
            ( { model | resources = Resources.update rMsg model.resources }, Cmd.none )


view : Model -> Html msg
view model =
    let
        renderParams =
            { camera = model.camera
            , time = model.time
            , size = ( model.width, model.height )
            }
    in
    Html.div
        [ HA.width model.width
        , HA.height model.height
        , HA.style "display" "block"
        ]
        [ Game.render renderParams (render model) ]


render : Model -> List Renderable
render model =
    [ renderPlayer model ]


renderPlayer : Model -> Renderable
renderPlayer model =
    Render.sprite
        { texture = Resources.getTexture playerTextureUrl model.resources
        , position = ( 0, 0 )
        , size = ( 2, 2 )
        }
