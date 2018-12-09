module WebSocket exposing ( createRoom, decodeMesage )

import Json.Encode as Encode
import Json.Decode as Decode

type alias SocketEvent =
    {
        eventType: String
      , playerName: String
      , room: String
      , data: String
    }

emptySocketEvent = 
  {
    eventType = ""
  , playerName = ""
  , room = ""
  , data = ""
  }

createRoom : String -> String
createRoom playerName =
  Encode.object
        [ ( "eventType", Encode.string "create" )
        , ( "playerName", Encode.string playerName )
        ]
  |> Encode.encode 0

decodeMesage: String -> SocketEvent
decodeMesage message =
  let 
    decoded = Decode.decodeString messageDecode message
  in 
    case decoded of
      Ok data ->
        data
      _ ->
        emptySocketEvent


messageDecode : Decode.Decoder SocketEvent
messageDecode =
  Decode.map4 SocketEvent
    (Decode.field "eventType" Decode.string)
    (Decode.field "playerName" Decode.string)
    (Decode.field "room" Decode.string)
    (Decode.field "data" Decode.string)