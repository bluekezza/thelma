port module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode exposing ((:=), andThen)
import Json.Encode
import String

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

port setStorage : Json.Encode.Value -> Cmd msg

port focus : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- JSON
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

-- MODEL
type Mode
    = Home
    | Edit
      
type alias Model =
    { mode : Mode
    }

emptyModel : Model
emptyModel =
  { mode = Home
  }

init : ( Model, Cmd Msg )
init = ( emptyModel, Cmd.none )

-- UPDATE
type Msg
    = NoOp
    | StartEdit
    | StopEdit

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []
    StartEdit ->
      { model | mode = Edit } ! []
    StopEdit ->
      { model | mode = Home } ! []

-- VIEW
view : Model -> Html Msg
view model =
  div
    [ class ""
    , style []
    ]
    [ text "thelma"
    , text ":"
    , text (toString model.mode)      
    ]

