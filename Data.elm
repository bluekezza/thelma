port module Data exposing (..)

import Json.Decode exposing ((:=), andThen)
import Json.Encode
import String

type alias ChannelName = String
type alias Headline = String
          
channels : List ChannelName
channels = [ "Home", "News" ]

type alias Article =
    { channel : ChannelName
    , headline : Headline
    }

{- JSON
port setStorage : Json.Encode.Value -> Cmd msg

-- ENCODE
modelToValue : Model -> Json.Encode.Value
modelToValue model =
    Json.Encode.object
        [ ("mode", modeToValue model.mode)
        ]

modeToValue : Mode -> Json.Encode.Value
modeToValue mode = Json.Encode.string (toString mode)

-- DECODE
modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    Json.Decode.object1 Model
        ("mode" := Json.Decode.string `andThen` modeDecoder)

modeDecoder : String -> Json.Decode.Decoder Mode
modeDecoder mode =
    case mode of
        "Home" -> Json.Decode.succeed Home
        "Edit" -> Json.Decode.succeed Edit
        _      -> Json.Decode.fail (mode ++ " is not a recognized value for Mode")
-}
