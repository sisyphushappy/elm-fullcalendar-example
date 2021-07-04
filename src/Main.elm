port module Main exposing (..)

import Browser
import Event
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Ports



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port sendMessage : Json.Encode.Value -> Cmd msg


port messageReceiver : (Json.Decode.Value -> msg) -> Sub msg



-- FLAGS


type alias Flags =
    {}



-- INIT


type alias Model =
    { event : Event.Model
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { event = Event.defaults Nothing }
    , Cmd.none
    )



-- UPDATE


type Msg
    = EventMsg Event.Msg
    | SubmittedEvent
    | ReceivedMessage Json.Decode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EventMsg sub ->
            let
                ( subModel, subCmd ) =
                    Event.update sub model.event
            in
            ( { model | event = subModel }
            , Cmd.map EventMsg subCmd
            )

        SubmittedEvent ->
            case Event.validate model.event of
                False ->
                    ( model, Cmd.none )

                True ->
                    ( model
                    , sendMessage <|
                        Ports.encodeSendMessage "ADD_EVENT" (Event.encode model.event)
                    )

        ReceivedMessage value ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver ReceivedMessage



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.div
            [ Html.Attributes.id "calendar" ]
            []
        , Event.view model.event |> Html.map EventMsg
        , Html.button
            [ Html.Events.onClick SubmittedEvent
            ]
            [ Html.text "Submit" ]
        ]
