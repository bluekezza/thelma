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
import RichText
import Monocle.Optional exposing (Optional)
import Monocle.Lens exposing (Lens)
import Debug exposing (log)

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
    { channel  : ChannelName
    , headline : Data.Headline
    , body     : RichText.Model
    }
      
type alias Model =
    { page : Page
    , article : Maybe Article
    }

-- Lens[start]
modelArticle : Optional Model Article
modelArticle =
    let
        getOption parent = parent.article
        set child parent = { parent | article = Just child }
    in
        Optional getOption set

articleChannel : Lens Article ChannelName
articleChannel =
    let
        get parent = parent.channel
        set child parent = { parent | channel = child }
    in
        Lens get set

articleHeadline : Lens Article Data.Headline
articleHeadline =
    let
        get parent = parent.headline
        set child parent = { parent | headline = child }
    in
        Lens get set

articleBody : Lens Article RichText.Model
articleBody =
    let
        get parent = parent.body
        set child parent = { parent | body = child }
    in
        Lens get set

modelArticleBody : Optional Model RichText.Model
modelArticleBody =
    modelArticle `Monocle.Optional.composeLens` articleBody
            
modelArticleChannel : Optional Model ChannelName
modelArticleChannel =
    modelArticle `Monocle.Optional.composeLens` articleChannel

channelMode : Lens ChannelName ControlMode
channelMode =
    let
        get parent = parent.mode
        set child parent = { parent | mode = child }
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
  , article =
      Just { channel =
                 { mode = ControlValue
                 , value = "News"
                 }
           , headline = "A headline acts like a summary"
           , body =
                 { paragraphs =
                       [{ markups = []
                        , name = "fc62"
                        , text = "The body comes..."
                        , style = RichText.Normal
                        }
                       ,{ markups = []
                        , name = "40f2"
                        , text = "...in..."
                        , style = RichText.Italic
                        }
                       ,{ markups = []
                        , name = "c13a"
                        , text = "...paragraphs"
                        , style = RichText.Large
                        }]
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
    | BodyMsg RichText.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case (log "msg: " msg) of
    NoOp ->
        model ! []
    StartEdit ->
        { model | page = PageEdit } ! []
    StopEdit ->
        { model | page = PageHome } ! []
    EditChannelName ->
        (model |> modelArticleChannelMode.set ControlEdit) ! []
    SetChannelName channelName ->
        (model |> modelArticleChannel.set { mode = ControlValue
                                          , value = channelName
                                          }) ! []
    BodyMsg a ->
        case modelArticleBody.getOption model of
            Nothing   -> model ! []
            Just body -> (model |> (modelArticleBody.set (RichText.update a body))) ! []

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

viewHeadline : Data.Headline -> Html Msg
viewHeadline headline =
    div
    [ class "headline"
    , contenteditable True
    , style [("display", "inline-block")]
    ]
    [ text headline ]

viewArticle : Maybe Article -> Html Msg
viewArticle mArticle =
    case mArticle of
        Nothing ->
            div [ class "edit" ]
                []
        Just article ->
            div [ class "edit" ]
                [ div [ class "meta"
                      , style [("background-color", "lightblue")]
                      ]
                      [ viewChannel article.channel ]
                , viewHeadline article.headline
                , RichText.view article.body |> App.map BodyMsg
                ]

viewRoot : Model -> Html Msg
viewRoot model =
  div
    [ class ""
    , style []
    ]
    [ div
      [ class "header"
      ]
      [ text (case model.page of
                  PageHome -> "Home"
                  PageEdit -> "Edit Article")
      ]
    , div
      [ class "content"
      ]
      [ viewArticle model.article ]
    , div
      [ class "footer"
      ]
      [ ]
    , div
      [ style [("display", "inline-block")]
      , width 200 ]
      [ button
        [ onClick (BodyMsg RichText.InsertSection) ]
        [ text "Insert Section" ]
      , button
        [ onClick (BodyMsg RichText.RemoveSection) ]
        [ text "Remove Section" ]
      , button
        [ onClick (BodyMsg RichText.UpdateSection) ]
        [ text "Update Section" ]
      , br [] []
      , button
        [ onClick (BodyMsg RichText.InsertParagraph) ]
        [ text "Insert Paragraph" ]
      , button
        [ onClick (BodyMsg RichText.RemoveParagraph) ]
        [ text "Remove Paragraph" ]
      , button
        [ onClick (BodyMsg RichText.UpdateParagraph) ]
        [ text "Update Paragraph" ]            
      ]
    ]
