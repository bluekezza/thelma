port module PlainText exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

type alias Model = String

sample : Model
sample = "This is a headline"

type Msg
    = Update
