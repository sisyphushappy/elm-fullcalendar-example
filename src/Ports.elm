module Ports exposing (..)

import Json.Encode


type alias ReceivePayload =
    {}


encodeSendMessage : String -> Json.Encode.Value -> Json.Encode.Value
encodeSendMessage messageType value =
    Json.Encode.object
        [ ( "type", Json.Encode.string messageType )
        , ( "value", value )
        ]
