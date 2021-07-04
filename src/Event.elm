module Event exposing (..)

import Bool.Extra as BoolX exposing (all)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Iso8601
import Json.Decode
import Json.Decode.Extra as DecodeX
import Json.Encode
import Maybe.Extra as MaybeX
import Time exposing (Posix)



-- MODEL


type alias Model =
    { maybeId : Maybe Int
    , title : String
    , description : String
    , startTimeString : String
    , endTimeString : String
    , maybeStartTime : Maybe Posix
    , maybeEndTime : Maybe Posix
    , error : String
    }


defaults : Maybe Int -> Model
defaults maybeId =
    { maybeId = maybeId
    , title = ""
    , description = ""
    , startTimeString = ""
    , endTimeString = ""
    , maybeStartTime = Nothing
    , maybeEndTime = Nothing
    , error = ""
    }
        |> setError



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
            ( { model | title = string } |> setError
            , Cmd.none
            )

        ChangedDescription string ->
            ( { model | description = string } |> setError
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
                        |> setError
                    , Cmd.none
                    )

                End ->
                    ( { model
                        | endTimeString = string
                        , maybeEndTime = maybeParsedTime
                      }
                        |> setError
                    , Cmd.none
                    )



-- VALIDATION


validate : Model -> Bool
validate model =
    BoolX.all
        [ not <| String.isEmpty model.title
        , not <| String.isEmpty model.description
        , MaybeX.isJust model.maybeStartTime
        , MaybeX.isJust model.maybeEndTime
        ]


setError : Model -> Model
setError model =
    case validate model of
        False ->
            { model | error = "ERROR: Event validation failed" }

        True ->
            { model | error = "" }



-- API


defaultTime : String
defaultTime =
    Iso8601.fromTime <| Time.millisToPosix 0


encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object
        [ ( "id", MaybeX.unwrap Json.Encode.null Json.Encode.int model.maybeId )
        , ( "title", Json.Encode.string model.title )
        , ( "description", Json.Encode.string model.description )
        , ( "start"
          , Json.Encode.string <|
                MaybeX.unwrap defaultTime Iso8601.fromTime model.maybeStartTime
          )
        , ( "end"
          , Json.Encode.string <|
                MaybeX.unwrap defaultTime Iso8601.fromTime model.maybeEndTime
          )
        ]

fullCalendarDecoder : Json.Decode.Decoder Model
fullCalendarDecoder =
    Json.Decode.succeed Model
        |> DecodeX.andMap
            (Json.Decode.field "id" <|
                Json.Decode.oneOf
                    [ Json.Decode.int
                        |> Json.Decode.andThen
                            (\id -> Json.Decode.succeed (Just id))
                    , Json.Decode.succeed Nothing
                    ]
            )
        |> DecodeX.andMap (Json.Decode.field "title" Json.Decode.string)
        |> DecodeX.andMap (Json.Decode.field "description" Json.Decode.string)
        |> DecodeX.andMap (Json.Decode.field "start" Json.Decode.string)
        |> DecodeX.andMap (Json.Decode.field "end" Json.Decode.string)
        |> DecodeX.andMap
            (Json.Decode.field "start"
                (Iso8601.decoder
                    |> Json.Decode.andThen
                            (\time ->
                                Json.Decode.succeed (Just time)
                            )
                )
            )
        |> DecodeX.andMap
            (Json.Decode.field "end"
                (Iso8601.decoder
                    |> Json.Decode.andThen
                            (\time ->
                                Json.Decode.succeed (Just time)
                            )
                )
            )
        |> DecodeX.andMap (Json.Decode.succeed "")

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
        , Html.br
            []
            []
        , Html.text model.error
        ]
