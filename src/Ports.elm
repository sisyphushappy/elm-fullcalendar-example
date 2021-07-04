module Ports exposing (..)

import Event
import Json.Decode
import Json.Encode


type ReceivePayload =
    ClickedEvent Event.Model

messageTypeKey : String
messageTypeKey = "type"

messageValueKey : String
messageValueKey = "value"


receivePayloadDecoder : Json.Decode.Decoder ReceivePayload
receivePayloadDecoder =
    Json.Decode.field messageTypeKey Json.Decode.string
        |> Json.Decode.andThen
            (
            \messageType ->
                case messageType of
                    "CLICKED_EVENT" ->
                        Json.Decode.field messageValueKey Event.fullCalendarDecoder
                            |> Json.Decode.andThen
                                (\event -> Json.Decode.succeed (ClickedEvent event))
                    _ -> Json.Decode.fail "Unrecognized message type"
            )

encodeSendMessage : String -> Json.Encode.Value -> Json.Encode.Value
encodeSendMessage messageType value =
    Json.Encode.object
        [ ( "type", Json.Encode.string messageType )
        , ( "value", value )
        ]
