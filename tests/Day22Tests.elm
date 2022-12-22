module Day22Tests exposing (..)

import AocUtil exposing (..)
import Day22 exposing (..)
import Day22Input exposing (input)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set exposing (Set)
import Dict exposing (Dict)
import String.Extra
import Test exposing (..)
import Tuple
import String as S
import List as L
import Debug as D
import Array exposing (Array)
import Regex

suite : Test
suite =
    describe "day 22"
        [ describe "part 2"
            [ test "input" <| \_ -> input |> part2 |> Expect.equal 11451
            ]
        ]
