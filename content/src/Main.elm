port module Main exposing (Msg(..), init, main, onState, subscriptions, update, view)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class)
import Model exposing (Model)



-- PORTS FROM JAVASCRIPT


port onState : (Model -> msg) -> Sub msg


init : Model -> ( Model, Cmd Msg )
init model =
    ( model
    , Cmd.none
    )


type Msg
    = NoOp
    | NewState Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewState newModel ->
            ( newModel, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        [ class "content"
        ]
        [ Html.text ("[Content App] clicks: " ++ String.fromInt model.clicks)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    onState NewState


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
