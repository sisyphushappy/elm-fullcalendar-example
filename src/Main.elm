module Main exposing (..)

import Browser
import Event
import Html exposing (Html)
import Html.Attributes



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- FLAGS


type alias Flags =
    {}



-- INIT


type alias Model =
    { event : Event.Model
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { event = Event.defaults 0 }
    , Cmd.none
    )



-- UPDATE


type Msg
    = EventMsg Event.Msg


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.div
            [ Html.Attributes.id "calendar" ]
            []
        , Event.view model.event |> Html.map EventMsg
        ]
