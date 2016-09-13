port module PlainText exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as Json
import Json.Encode

type alias Model = String

type Msg
    = Update String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Update v -> v

view : String -> Html Msg
view model =
    div
      []
      [ h3
        [ contenteditable True
        , onInput Update
        , style [("width", "100%")]
        , property "textContent" (Json.Encode.string model)
        ]
        [ ]
      ]

onInput : (String -> msg) -> Attribute msg
onInput tagger =
    Html.Events.on "input" (Json.map tagger targetTextContent)
            
targetTextContent : Json.Decoder String
targetTextContent =
    Json.at ["target", "textContent"] Json.string

