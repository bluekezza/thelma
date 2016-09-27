port module Thelma.Html.Events exposing (onInput)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as Json


onInput : (String -> msg) -> Attribute msg
onInput tagger =
    Html.Events.on "input" (Json.map tagger targetTextContent)


targetTextContent : Json.Decoder String
targetTextContent =
    Json.at [ "target", "textContent" ] Json.string
