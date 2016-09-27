port module Thelma.Html exposing (editable)

import Html exposing (..)
import Html.Attributes exposing (..)
import Thelma.Html.Events exposing (onInput)
import Json.Encode


editable : String -> List (Attribute a) -> (String -> a) -> String -> Html a
editable tagName attrs tagger initialValue =
    let
        attrs' =
            attrs
                `List.append`
                    [ contenteditable True
                    , property "textContent" (Json.Encode.string initialValue)
                    , onInput tagger
                    ]
    in
        node tagName attrs' []
