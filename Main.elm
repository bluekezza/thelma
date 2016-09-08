port module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import String

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

port setStorage : Model -> Cmd msg

port focus : String -> Cmd msg

-- MODEL
type alias Model =
    { userName : String
    }

emptyModel : Model
emptyModel =
  { userName = ""
  }

init : ( Model, Cmd Msg )
init = ( emptyModel, Cmd.none )

-- UPDATE
type Msg
    = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

-- VIEW
view : Model -> Html Msg
view model =
  div
    [ class ""
    , style []
    ]
    [ text "thelma"
    ]
