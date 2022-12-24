module Day24Tests exposing (..)

import AocUtil exposing (..)
import Array exposing (Array)
import Day24 exposing (..)
import Day24Input exposing (..)
import Debug as D
import Dict exposing (Dict)
import Expect
import Fuzz
import List as L
import List.Extra as LE
import Regex
import Set exposing (Set)
import String as S
import String.Extra
import String.Interpolate exposing (interpolate)
import Test exposing (..)
import Tuple


example1 =
    """#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#"""


example2 =
    """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"""


suite : Test
suite =
    describe "day 24"
        [ describe "part 1"
            [ test "example1" <| \_ -> example1 |> part1 |> Expect.equal (Just 10)
            , test "example2" <| \_ -> example2 |> part1 |> Expect.equal (Just 18)
            , test "input" <| \_ -> input |> part1 |> Expect.equal (Just 332)
            ]
        , describe "part 2"
            [ test "example1" <| \_ -> example1 |> part2 |> Expect.equal (Just 30)
            , test "example2" <| \_ -> example2 |> part2 |> Expect.equal (Just 54)
            , test "input" <| \_ -> input |> part2 |> Expect.equal (Just 942)
            ]
        ]
