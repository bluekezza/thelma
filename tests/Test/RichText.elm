module Test.RichText exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import RichText exposing (..)
import ElmTest exposing (..)


type alias Fixture =
    ( String, Paragraph, Html Msg )


fixtures : List Fixture
fixtures =
    let
        noMarkups =
            [ ( "Normal, no markups"
              , { name = "A"
                , style = Normal
                , text = "b"
                , markups = []
                }
              , p [ name "A" ] [ text "b" ]
              )
            , ( "Italic, no markups"
              , { name = "A"
                , style = Italic
                , text = "b"
                , markups = []
                }
              , em [ name "A" ] [ text "b" ]
              )
            , ( "Large, no markups"
              , { name = "A"
                , style = Large
                , text = "b"
                , markups = []
                }
              , h3 [ name "A" ] [ text "b" ]
              )
            , ( "Bold, no markups"
              , { name = "A"
                , style = Bold
                , text = "b"
                , markups = []
                }
              , strong [ name "A" ] [ text "b" ]
              )
            ]
    in
        noMarkups


runner : ( String, Paragraph, Html Msg ) -> Test
runner ( scenario, paragraph, expectedOut ) =
    let
        actualOut =
            viewParagraph paragraph
    in
        test scenario <| assertEqual expectedOut actualOut


sampleHtml : Html Msg
sampleHtml =
    p
        []
        [ text "Can I do "
          -- 0,8
        , strong
            []
            [ text "bo..."
              -- 9,12
            , em
                []
                [ text "with italics" ]
              -- 12,24
            , text "...ld"
            ]
        ]


sampleParagraph =
    { name = "b4ec"
    , style = Normal
    , text = "Can I do bo...with italics...ld please"
    , markups =
        [ { style = Bold
          , start = 9
          , end = 31
          }
        , { style = Italic
          , start = 14
          , end = 26
          }
        ]
    }


tests : Test
tests =
    suite "A Test Suite" <|
        List.map runner fixtures
