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
import Monocle.Optional exposing (Optional)
import Monocle.Lens exposing (Lens)

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

-- Lens[start]
modelArticle : Optional Model Article
modelArticle =
    let
        getOption m = m.article
        set a m = { m | article = Just a }
    in
        Optional getOption set

articleChannel : Lens Article ChannelName
articleChannel =
    let
        get a = a.channel
        set c a = { a | channel = c }
    in
        Lens get set

modelArticleChannel : Optional Model ChannelName
modelArticleChannel =
    modelArticle `Monocle.Optional.composeLens` articleChannel

channelMode : Lens ChannelName ControlMode
channelMode =
    let
        get c = c.mode
        set m c = { c | mode = m }
    in
        Lens get set

modelArticleChannelMode : Optional Model ControlMode
modelArticleChannelMode =
    modelArticleChannel `Monocle.Optional.composeLens` channelMode
-- Lens[end]
    
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
            model' = model |> modelArticleChannelMode.set ControlEdit
        in
            model' ! []
    SetChannelName channelName ->
        let
            model' = model |> modelArticleChannel.set { mode = ControlValue
                                                      , value = channelName
                                                      }
        in
            model' ! []

-- VIEW
viewChannel : ChannelName -> Html Msg
viewChannel channel =
    let
        className = "channelName"
    in
        case channel.mode of
            ControlValue -> div
                            [ class className
                            , onClick EditChannelName
                            , style [("display", "inline-block")]
                            ]
                           [ text channel.value ]
            ControlEdit -> div
                           [ class className
                           , style [("display", "inline-block")] ]
                           [ ol
                             []
                             (List.map (\c -> li
                                            [ onClick (SetChannelName c)
                                            , class "option"
                                            , style [("display", "online")]
                                            ]
                                            [ text c ])
                                  Data.channels)]

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
