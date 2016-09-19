port module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode exposing ((:=), andThen)
import Json.Encode
import String
import Data
import RichText
import PlainText
import Monocle.Optional exposing (Optional)
import Monocle.Lens exposing (Lens)
import Debug exposing (log)


main : Program Never
main =
    Html.program
        { init = init
        , view = viewRoot
        , update = update
        , subscriptions = subscriptions
        }


port focus : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
    { channel : ChannelName
    , headline : Data.Headline
    , body : RichText.Model
    }


type alias Model =
    { page : Page
    , article : Maybe Article
    }



-- Lens[start]


modelArticle : Optional Model Article
modelArticle =
    let
        getOption parent =
            parent.article

        set child parent =
            { parent | article = Just child }
    in
        Optional getOption set


articleChannel : Lens Article ChannelName
articleChannel =
    let
        get parent =
            parent.channel

        set child parent =
            { parent | channel = child }
    in
        Lens get set


articleHeadline : Lens Article Data.Headline
articleHeadline =
    let
        get parent =
            parent.headline

        set child parent =
            { parent | headline = child }
    in
        Lens get set


articleBody : Lens Article RichText.Model
articleBody =
    let
        get parent =
            parent.body

        set child parent =
            { parent | body = child }
    in
        Lens get set


modelArticleChannel : Optional Model ChannelName
modelArticleChannel =
    modelArticle `Monocle.Optional.composeLens` articleChannel


modelArticleHeadline : Optional Model PlainText.Model
modelArticleHeadline =
    modelArticle `Monocle.Optional.composeLens` articleHeadline


modelArticleBody : Optional Model RichText.Model
modelArticleBody =
    modelArticle `Monocle.Optional.composeLens` articleBody


channelMode : Lens ChannelName ControlMode
channelMode =
    let
        get parent =
            parent.mode

        set child parent =
            { parent | mode = child }
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
        Just
            { channel =
                { mode = ControlValue
                , value = "News"
                }
            , headline = "A headline acts like a summary"
            , body =
                { paragraphs =
                    [ { markups = []
                      , name = "fc62"
                      , text = "The body comes..."
                      , style = RichText.Normal
                      }
                    , { markups = []
                      , name = "40f2"
                      , text = "...in..."
                      , style = RichText.Italic
                      }
                    , { markups = []
                      , name = "c13a"
                      , text = "...paragraphs"
                      , style = RichText.Large
                      }
                    ]
                }
            }
    }


init : ( Model, Cmd Msg )
init =
    ( testModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | StartEdit
    | StopEdit
    | EditChannelName
    | SetChannelName Data.ChannelName
    | BodyMsg RichText.Msg
    | HeadlineMsg PlainText.Msg


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
            (model
                |> modelArticleChannel.set
                    { mode = ControlValue
                    , value = channelName
                    }
            )
                ! []

        BodyMsg a ->
            Monocle.Optional.modify modelArticleBody (RichText.update a) model ! []

        HeadlineMsg a ->
            Monocle.Optional.modify modelArticleHeadline (PlainText.update a) model ! []



-- VIEW


viewChannel : ChannelName -> List (Html Msg)
viewChannel channel =
    let
        className =
            "channelName"
    in
        case channel.mode of
            ControlValue ->
                [ div
                    [ class className
                    , onClick EditChannelName
                    , style [ ( "display", "inline-block" ) ]
                    ]
                    [ text channel.value ]
                ]

            ControlEdit ->
                [ div
                    [ class className
                    , onClick (SetChannelName channel.value)
                    , style [ ( "display", "inline-block" ) ]
                    ]
                    [ text channel.value ]
                , div
                    [ class className
                    , style
                        [ ( "display", "block" )
                        , ( "position", "absolute" )
                        ]
                    ]
                    [ ol
                        [ class "options" ]
                        (List.map
                            (\c ->
                                li
                                    [ onClick (SetChannelName c)
                                    , class "option"
                                    , style [ ( "display", "block" ) ]
                                    ]
                                    [ text c ]
                            )
                            Data.channels
                        )
                    ]
                ]


viewHeadline : String -> Html Msg
viewHeadline headline =
    div
        [ class "headline"
        , name "headline"
        ]
        [ PlainText.view headline |> Html.map HeadlineMsg ]


viewBody : RichText.Model -> Html Msg
viewBody body =
    RichText.view body |> Html.map BodyMsg


viewArticle : Maybe Article -> Html Msg
viewArticle mArticle =
    case mArticle of
        Nothing ->
            div [ class "edit" ]
                []

        Just article ->
            div [ class "edit" ]
                [ div [ class "meta" ]
                    (viewChannel article.channel)
                , viewHeadline article.headline
                , viewBody article.body
                , br [] []
                , text (toString article)
                ]


viewRoot : Model -> Html Msg
viewRoot model =
    div
        [ class ""
        , style []
        ]
        [ div
            [ class "header" ]
            []
        , div
            [ class "content" ]
            [ viewArticle model.article ]
        , div
            [ class "footer"
            ]
            []
        ]
