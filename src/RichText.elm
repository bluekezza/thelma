port module RichText exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List exposing (map, concat)
import Thelma.Html exposing (editable)


type Msg
    = InsertParagraph
    | RemoveParagraph
    | UpdateParagraph Int String


type Style
    = Normal
    | Italic
    | Large


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


viewParagraph : Int -> Paragraph -> Html Msg
viewParagraph index paragraph =
    let
        tag =
            case paragraph.style of
                Normal ->
                    "p"

                Large ->
                    "h3"

                Italic ->
                    "i"
    in
        editable tag
            []
            (UpdateParagraph index)
            paragraph.text


view : Model -> Html Msg
view { paragraphs } =
    div [ class "section" ]
        (List.indexedMap viewParagraph paragraphs)


update : Msg -> Model -> Model
update msg model =
    case msg of
        InsertParagraph ->
            model

        RemoveParagraph ->
            model

        UpdateParagraph index a ->
            let
                editAtIndex i v =
                    if i == index then
                        { v | text = a }
                    else
                        v

                paragraphs' =
                    List.indexedMap editAtIndex model.paragraphs
            in
                { model | paragraphs = paragraphs' }
