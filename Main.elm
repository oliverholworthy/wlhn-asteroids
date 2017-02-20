module Main exposing (..)

import AnimationFrame
import Char
import Html
import Keyboard
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)


type alias Coords =
    { x : Float
    , y : Float
    }


type alias Player =
    { position : Coords
    , velocity : Coords
    }


type KeyState
    = Pressed
    | Unpressed


type Key
    = Up
    | Down
    | Left
    | Right


type alias Keys =
    { up : KeyState
    , down : KeyState
    , left : KeyState
    , right : KeyState
    }


type alias Model =
    { player : Player
    , keys : Keys
    }


type Msg
    = Noop
    | KeyChange Key KeyState
    | TimeStep Time


init : ( Model, Cmd Msg )
init =
    ( { player =
            { position = { x = 10, y = 10 }
            , velocity = { x = 0, y = 0 }
            }
      , keys =
            { up = Unpressed
            , down = Unpressed
            , left = Unpressed
            , right = Unpressed
            }
      }
    , Cmd.none
    )


mapSize =
    100.0


viewBackground =
    rect
        [ x "0"
        , y "0"
        , width (toString mapSize)
        , height (toString mapSize)
        , rx "0"
        , ry "0"
        ]
        []


viewPlayer player =
    circle
        [ cx (toString player.position.x)
        , cy (toString player.position.y)
        , r "5"
        , Svg.Attributes.style "fill:white"
        ]
        []


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ svg
            [ width "800"
            , height "600"
            , viewBox "0 0 120 120"
            ]
            [ viewBackground
            , viewPlayer model.player
            ]
        ]


handleKeyChange : Key -> KeyState -> Model -> Model
handleKeyChange key keystate model =
    let
        keys =
            model.keys

        newKeys =
            case key of
                Up ->
                    { keys | up = keystate }

                Down ->
                    { keys | down = keystate }

                Left ->
                    { keys | left = keystate }

                Right ->
                    { keys | right = keystate }
    in
        { model | keys = newKeys }


handleTimeStep : Time -> Model -> Model
handleTimeStep diff model =
    let
        player =
            model.player

        millis =
            Time.inSeconds diff

        factor =
            100

        dampingFactor =
            factor / 10

        damp x =
            -1 * (sign x) * dampingFactor

        sign x =
            if x == 0 then
                x
            else
                x / (abs x)

        a =
            { y =
                case ( model.keys.up, model.keys.down ) of
                    ( Pressed, Pressed ) ->
                        0

                    ( Unpressed, Unpressed ) ->
                        damp model.player.velocity.y

                    ( Pressed, _ ) ->
                        -1 * factor

                    ( _, Pressed ) ->
                        factor
            , x =
                case ( model.keys.left, model.keys.right ) of
                    ( Pressed, Pressed ) ->
                        0

                    ( Unpressed, Unpressed ) ->
                        damp model.player.velocity.x

                    ( Pressed, _ ) ->
                        -1 * factor

                    ( _, Pressed ) ->
                        factor
            }

        maxV =
            mapSize

        updateVelocity v =
            { x =
                (a.x * millis + v.x)
                    |> clamp (-1 * maxV) maxV
            , y =
                (a.y * millis + v.y)
                    |> clamp (-1 * maxV) maxV
            }

        updatePosition p v =
            { x = v.x * millis + p.x, y = v.y * millis + p.y }

        wrapCoord p =
            if p > mapSize then
                p - mapSize
            else if p < 0 then
                p + mapSize
            else
                p

        wrapPosition p =
            { x = wrapCoord p.x
            , y = wrapCoord p.y
            }

        newVelocity =
            updateVelocity model.player.velocity

        newPosition =
            updatePosition model.player.position newVelocity
                |> wrapPosition

        newPlayer =
            { player | velocity = newVelocity, position = newPosition }
    in
        { model | player = newPlayer }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                KeyChange key keystate ->
                    handleKeyChange key keystate model

                TimeStep diff ->
                    handleTimeStep diff model

                Noop ->
                    model
    in
        ( newModel, Cmd.none )


keyCodeToMsg : KeyState -> Keyboard.KeyCode -> Msg
keyCodeToMsg keystate keyCode =
    let
        _ =
            Debug.log (toString keyCode) 1
    in
        case keyCode of
            40 ->
                KeyChange Down keystate

            38 ->
                KeyChange Up keystate

            39 ->
                KeyChange Right keystate

            37 ->
                KeyChange Left keystate

            _ ->
                Noop


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs (keyCodeToMsg Pressed)
        , Keyboard.ups (keyCodeToMsg Unpressed)
        , AnimationFrame.diffs TimeStep
        ]


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
