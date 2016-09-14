port module PlainText exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as Json
import Json.Encode
import Thelma.Html exposing (editable)

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
      [ editable
        "h3"
          [style [("width", "100%")]]
          Update model
      ]
