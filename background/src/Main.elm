port module Main exposing (Msg(..), broadcast, clicked, init, main, subscriptions, update)

import Html exposing (div)
import Json.Decode
import Model exposing (Model)
import Platform



-- PORTS FROM JAVASCRIPT


port clicked : (() -> msg) -> Sub msg



-- PORTS TO JAVASCRIPT


port broadcast : Model -> Cmd msg



-- MODEL


init : Model -> ( Model, Cmd Msg )
init model =
    ( model
    , Cmd.none
    )


type Msg
    = NoOp
    | Click


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Click ->
            let
                nextModel =
                    { model | clicks = model.clicks + 1 }
            in
            ( nextModel, broadcast nextModel )


subscriptions : Model -> Sub Msg
subscriptions model =
    clicked (\_ -> Click)


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
