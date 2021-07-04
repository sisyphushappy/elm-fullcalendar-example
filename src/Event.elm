module Event exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Iso8601
import Time exposing (Posix)



-- MODEL


type alias Model =
    { id : Int
    , title : String
    , description : String
    , startTimeString : String
    , endTimeString : String
    , maybeStartTime : Maybe Posix
    , maybeEndTime : Maybe Posix
    }


defaults : Int -> Model
defaults id =
    { id = id
    , title = ""
    , description = ""
    , startTimeString = ""
    , endTimeString = ""
    , maybeStartTime = Nothing
    , maybeEndTime = Nothing
    }



-- UPDATE


type Msg
    = ChangedTitle String
    | ChangedDescription String
    | ChangedDate DateType String


type DateType
    = Start
    | End


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedTitle string ->
            ( { model | title = string }
            , Cmd.none
            )

        ChangedDescription string ->
            ( { model | description = string }
            , Cmd.none
            )

        ChangedDate dateType string ->
            let
                maybeParsedTime =
                    case Iso8601.toTime string of
                        Err _ ->
                            Nothing

                        Ok posix ->
                            Just posix
            in
            case dateType of
                Start ->
                    ( { model
                        | startTimeString = string
                        , maybeStartTime = maybeParsedTime
                      }
                    , Cmd.none
                    )

                End ->
                    ( { model
                        | endTimeString = string
                        , maybeEndTime = maybeParsedTime
                      }
                    , Cmd.none
                    )



-- VIEW


view : Model -> Html Msg
view model =
    let
        viewTimeInput :
            { placeholder : String
            , textValue : String
            , maybeTime : Maybe Posix
            , msg : String -> Msg
            }
            -> Html Msg
        viewTimeInput config =
            Html.div
                []
                [ Html.input
                    [ Html.Attributes.type_ "text"
                    , Html.Attributes.class "input"
                    , Html.Attributes.placeholder config.placeholder
                    , Html.Attributes.value config.textValue
                    , Html.Events.onInput config.msg
                    ]
                    []
                , case config.maybeTime of
                    Nothing ->
                        Html.text "Enter a valid ISO-8601 time"

                    Just time ->
                        Html.text <| Iso8601.fromTime time
                ]
    in
    Html.div
        []
        [ Html.h3
            []
            [ Html.text "Create / Edit Event"
            ]
        , Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.class "input"
            , Html.Attributes.placeholder "Title"
            , Html.Attributes.value model.title
            , Html.Events.onInput ChangedTitle
            ]
            []
        , Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.class "input"
            , Html.Attributes.placeholder "Description"
            , Html.Attributes.value model.description
            , Html.Events.onInput ChangedDescription
            ]
            []
        , viewTimeInput
            { placeholder = "Start Time"
            , textValue = model.startTimeString
            , maybeTime = model.maybeStartTime
            , msg = ChangedDate Start
            }
        , viewTimeInput
            { placeholder = "End Time"
            , textValue = model.endTimeString
            , maybeTime = model.maybeEndTime
            , msg = ChangedDate End
            }
        ]
