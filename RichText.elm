port module RichText exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import List exposing (map, concat)
import Thelma.Html.Events exposing (onInput)

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
    div [ class "section" ]
        (List.append
          (List.map viewParagraph paragraphs)
          [div []
               [text (toString paragraphs)]
          ])

update : Msg -> Model -> Model
update msg model =
    case msg of
        InsertParagraph   -> model
        RemoveParagraph   -> model
        UpdateParagraph _ -> model
