module Main exposing (..)

import ElmTest exposing (..)
import Test.RichText as RichText


tests : Test
tests =
    suite "Thelma Tests"
        [ RichText.tests ]


main : Program Never
main =
    runSuite tests
