module Day10 exposing (..)

import AocUtil exposing (..)
import Day10Input exposing (input)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set
import String.Extra
import Test exposing (..)
import Tuple
import String as S
import List as L
import Debug as D
import Array exposing (Array)


example1 =
    """

"""


suite : Test
suite =
    describe "day 10"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> Expect.equal 0
            , test "input" <| \_ -> input |> part1 |> Expect.equal 0
            ]
        , skip <| describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 0
            , test "input" <| \_ -> input |> part2 |> Expect.equal 0
            ]
        ]


part1 input =
    let
        data =
            input
            |> S.lines
    in
    0

part2: String -> Int
part2 input = 0
