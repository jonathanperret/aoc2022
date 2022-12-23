module Day23Tests exposing (..)

import AocUtil exposing (..)
import Day23Input exposing (..)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set exposing (Set)
import String.Extra
import Test exposing (..)
import Tuple
import String as S
import List as L
import Debug as D
import Array exposing (Array)
import Regex
import String.Interpolate exposing (interpolate)
import Day23 exposing (..)


suite : Test
suite =
    describe "day 23"
        [ describe "part 1"
            [ test "example0" <| \_ -> example0 |> part1 |> Expect.equal 25
            , test "example1" <| \_ -> example1 |> part1 |> Expect.equal 110
            , test "input" <| \_ -> input |> part1 |> Expect.equal 4181
            ]
        , describe "part 2"
            [ test "example1" <| \_ -> example1 |> part2 |> Expect.equal 20
            , test "input" <| \_ -> input |> part2 |> Expect.equal 973
            ]
        ]
