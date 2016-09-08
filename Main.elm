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
    , view = viewRoot
    , update = update
    , subscriptions = subscriptions
    }

port focus : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- MODEL
type Mode
    = Home
    | Edit

type alias ChannelName = String
          
channels : List ChannelName
channels = [ "Home", "News" ]
                     
type alias Article =
    { channel : ChannelName }
      
type alias Model =
    { mode : Mode
    , article : Maybe Article
    }

emptyModel : Model
emptyModel =
  { mode = Home
  , article = Nothing
  }

testModel : Model
testModel =
  { mode = Edit
  , article = Just { channel = "News" }
  }
    
init : ( Model, Cmd Msg )
init = ( testModel, Cmd.none )

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
viewChannel : ChannelName -> Html Msg
viewChannel channel = text channel

viewEdit : Maybe Article -> Html Msg
viewEdit mArticle =
    case mArticle of
        Nothing ->
            div [ class "edit" ]
                []
        Just article ->
            div [ class "edit" ]
                [ viewChannel article.channel ]

viewRoot : Model -> Html Msg
viewRoot model =
  div
    [ class ""
    , style []
    ]
    [ div
      [ class "header"
      ]
      [ text "thelma"
      , text (toString model.mode)
      ]
    , div
      [ class "content"
      ]
      [ viewEdit model.article
      ]
    , div
      [ class "footer"
      ]
      [ ]
    ]
    
