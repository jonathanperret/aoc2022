module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz


suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse"
            [ test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"
            ]
        ]
