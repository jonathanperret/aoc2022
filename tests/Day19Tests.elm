module Day19Tests exposing (..)

import AocUtil exposing (..)
import Day19Input exposing (..)
import Day19 exposing (..)
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
import AStar.Generalised exposing (findPath)

import Dict exposing (Dict)
import Set exposing (Set)

suite : Test
suite =
    describe "day 19"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> Expect.equal 33
            , test "input" <| \_ -> input |> part1 |> Expect.equal 1624
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal (56*62)
            , test "input" <| \_ -> input |> part2 |> Expect.equal 12628
            ]
        ]
