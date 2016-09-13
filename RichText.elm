port module RichText exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (map, concat)

type Msg
    = InsertParagraph
    | RemoveParagraph
    | UpdateParagraph String

type Style
    = Normal
    | Italic
    | Large

type alias Markup =
    { style : Style
    , start : Int
    , end   : Int
    }

type alias Paragraph =
    { markups : List Markup
    , name    : String
    , text    : String
    , style   : Style
    }

type alias Model =
    { paragraphs : List Paragraph
    }

viewParagraph : Paragraph -> Html Msg
viewParagraph paragraph =
    let
        tag = case paragraph.style of
                  Normal -> p
                  Large  -> h3
                  Italic -> i
    in 
        tag [ name paragraph.name
            , contenteditable True
            , onInput UpdateParagraph ]
            [ text paragraph.text ]
    
view : Model -> Html Msg
view { paragraphs } =
    let
        foo = 1
    in
        div [ class "section" ]
            (List.map viewParagraph paragraphs)

update : Msg -> Model -> Model
update msg model =
    case msg of
        InsertParagraph   -> model
        RemoveParagraph   -> model
        UpdateParagraph _ -> model
