port module RichText exposing (..)

import String as String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List exposing (map, concat)
import Thelma.Html exposing (editable)


type Xml
    = Text String
    | Node String (List ( String, String )) (List Xml)


type Tag
    = Image Int


viewImage : Int -> Html Msg
viewImage id =
    img
        [ attribute "data-id" (toString id)
        , src "http://i.dailymail.co.uk/i/pix/2016/09/08/17/18D7D2DA00000578-3780194-image-a-27_1473352833124.jpg"
        ]
        []


viewTag : Tag -> Html Msg
viewTag tag =
    case tag of
        Image id ->
            viewImage id


type Msg
    = InsertParagraph
    | RemoveParagraph
    | UpdateParagraph String String
    | InsertTag Tag


type Style
    = Normal
    | Italic
    | Large
    | Bold


type alias Markup =
    { style : Style
    , start : Int
    , end : Int
    }


type alias Paragraph =
    { markups : List Markup
    , name : String
    , text : String
    , style : Style
    }


type alias Model =
    { paragraphs : List Paragraph
    }


styleToTag : Style -> String
styleToTag style =
    case style of
        Normal ->
            "p"

        Large ->
            "h3"

        Italic ->
            "em"

        Bold ->
            "strong"


viewParagraph : Paragraph -> Html Msg
viewParagraph paragraph =
    let
        tag =
            styleToTag paragraph.style
    in
        node tag
            [ name paragraph.name ]
            [ text paragraph.text ]


view : Model -> Html Msg
view { paragraphs } =
    article
        [ class "body"
        , contenteditable True
        ]
        (List.map viewParagraph paragraphs)


update : Msg -> Model -> Model
update msg model =
    case msg of
        InsertParagraph ->
            model

        RemoveParagraph ->
            model

        UpdateParagraph name a ->
            let
                editAtName n v =
                    if n == v.name then
                        { v | text = a }
                    else
                        v

                paragraphs' =
                    List.map (editAtName name) model.paragraphs
            in
                { model | paragraphs = paragraphs' }

        InsertTag tag ->
            case tag of
                Image id ->
                    model
