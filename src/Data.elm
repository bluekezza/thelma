port module Data exposing (..)

import Json.Decode exposing ((:=), andThen)
import Json.Encode
import String


type alias ChannelName =
    String


type alias Headline =
    String


channels : List ChannelName
channels =
    [ "Home", "News" ]


type alias Article =
    { channel : ChannelName
    , headline : Headline
    }
