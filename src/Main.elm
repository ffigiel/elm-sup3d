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
import Keyboard
import Keyboard.Arrows
import Task


type alias Model =
    { time : Float
    , camera : Camera
    , width : Int
    , height : Int
    , resources : Resources
    , keys : List Keyboard.Key
    }


type Msg
    = Tick Float
    | GotViewport Int Int
    | Resized
    | GotResources Resources.Msg
    | KeyPress Keyboard.Msg


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


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        model : Model
        model =
            { time = 0
            , width = 0
            , height = 0
            , camera = Camera.fixedArea (16 * 9) ( 0, 0 )
            , resources = Resources.init
            , keys = []
            }

        loadResources =
            Resources.loadTextures
                [ textures.player
                , textures.grass
                , textures.water
                ]
                |> Cmd.map GotResources

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
        , Sub.map KeyPress Keyboard.subscriptions
        , Browser.Events.onResize (\_ _ -> Resized)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick d ->
            { model | time = model.time + d }
                |> tick

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

        KeyPress kMsg ->
            let
                keys =
                    Keyboard.update kMsg model.keys
            in
            ( { model | keys = keys }, Cmd.none )


tick : Model -> ( Model, Cmd Msg )
tick model =
    ( model, Cmd.none )


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
    renderPlayer model
        :: renderBackground model


renderBackground : Model -> List Renderable
renderBackground model =
    let
        tile texture x y =
            Render.sprite
                { texture = Resources.getTexture texture model.resources
                , position = ( x, y )
                , size = unitSize
                }

        g =
            tile textures.grass

        w =
            tile textures.water
    in
    renderTileMap
        [ [ g, g, g, g, g, g, g, g, w, w ]
        , [ g, g, g, g, g, w, w, w, w, g ]
        , [ g, g, g, w, w, w, w, g, g, g ]
        , [ g, g, g, g, g, g, w, w, w, g ]
        , [ g, g, g, g, g, g, g, w, w, w ]
        ]


renderTileMap : List (List (Float -> Float -> Renderable)) -> List Renderable
renderTileMap ll =
    let
        renderRow y =
            List.indexedMap (renderTile y)

        renderTile y x r =
            r (toFloat x) (toFloat (List.length ll - y - 1))
    in
    ll
        |> List.indexedMap renderRow
        |> List.concat


renderPlayer : Model -> Renderable
renderPlayer model =
    Render.sprite
        { texture = Resources.getTexture textures.player model.resources
        , position = ( 0, 0 )
        , size = unitSize
        }
