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
import Data

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
type Page
    = PageHome
    | PageEdit

type ControlMode
    = ControlEdit
    | ControlValue

type alias ChannelName =
    { mode : ControlMode
    , value : Data.ChannelName
    }
        
type alias Article =
    { channel : ChannelName }
      
type alias Model =
    { page : Page
    , article : Maybe Article
    }

emptyModel : Model
emptyModel =
  { page = PageHome
  , article = Nothing
  }

testModel : Model
testModel =
  { page = PageEdit  
  , article = Just { channel = { mode = ControlValue
                               , value = "News"
                               }
                   }
  }

init : ( Model, Cmd Msg )
init = ( testModel, Cmd.none )

-- UPDATE
type Msg
    = NoOp
    | StartEdit
    | StopEdit
    | EditChannelName
    | SetChannelName Data.ChannelName

updateArticleChannelMode : ControlMode -> Article -> Article
updateArticleChannelMode channelMode article =
    let
        channel = article.channel
    in
        { article | channel = { channel | mode = channelMode }}

setArticleChannelValue : Data.ChannelName -> Article -> Article
setArticleChannelValue channelName article =
    let
        channel = article.channel
    in
        { article | channel = { channel | mode = ControlValue
                                        , value = channelName }}
      
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
        model ! []
    StartEdit ->
        { model | page = PageEdit } ! []
    StopEdit ->
        { model | page = PageHome } ! []
    EditChannelName ->
        let
            mArticle = model.article
        in
            { model | article = Maybe.map (updateArticleChannelMode ControlEdit) mArticle } ! []
    SetChannelName channelName ->
        let
            mArticle = model.article
        in
            { model | article = Maybe.map (setArticleChannelValue channelName) mArticle } ! []

-- VIEW
viewChannel : ChannelName -> Html Msg
viewChannel channel =
    case channel.mode of
        ControlValue -> div [ onClick EditChannelName ]
                            [ text channel.value ]
        ControlEdit -> div [] [ ol [] (List.map (\c -> li [ onClick (SetChannelName c)]
                                                          [ text c ]) Data.channels)]
                              

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
      , text (toString model.page)
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
    
