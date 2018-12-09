module WebSocket exposing ( createRoom, markSquare, joinRoom, decodeMesage, restart, GameData )

import Json.Encode as Encode
import Json.Decode as Decode

type alias SocketEvent =
  {
      eventType: String
    , playerName: String
    , room: String
    , data: String
  }

type alias GameData =
  {
      enventType: String
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

markSquare: Int -> Int -> String -> String
markSquare x y roomId = 
  Encode.object
        [ ( "eventType", Encode.string "data" )
        , ( "room", Encode.string roomId )
        , ( "data", Encode.string (String.fromInt(x) ++ "," ++ String.fromInt(y)) )
        ]
  |> Encode.encode 0

restart: String -> String
restart roomId =
  Encode.object
        [ ( "eventType", Encode.string "data" )
        , ( "room", Encode.string roomId ) 
        , ( "data", Encode.string "restart" )
        ]
  |> Encode.encode 0

joinRoom : String -> String -> String
joinRoom roomId playerName =
  Encode.object
        [ ( "eventType", Encode.string "join" )
        , ( "playerName", Encode.string playerName )
        , ( "room", Encode.string roomId )
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