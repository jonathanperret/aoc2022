module Day4Tests exposing (..)

import AocUtil exposing (..)
import Day4Input exposing (input)
import Expect
import Fuzz
import List.Extra
import Parser as P exposing ((|.), (|=), Parser)
import Set
import Test exposing (..)
import Tuple


example1 =
    """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""


suite : Test
suite =
    describe "day 4"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> Expect.equal 2
            , test "input" <| \_ -> input |> part1 |> Expect.equal 496
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 4
            , test "input" <| \_ -> input |> part2 |> Expect.equal 847
            ]
        ]


type alias Range =
    ( Int, Int )


rangePairs =
    let
        range =
            P.succeed Tuple.pair
                |= P.int
                |. P.symbol "-"
                |= P.int

        rangePair =
            P.succeed Tuple.pair
                |= range
                |. P.symbol ","
                |= range
    in
    P.oneOf
        [ P.succeed (::)
            |= rangePair
            |. P.spaces
            |= P.lazy (\_ -> rangePairs)
        , P.succeed [] |. P.end
        ] |> P.map List.reverse


parse : String -> List ( Range, Range )
parse = P.run rangePairs >> fromOk


part1 : String -> Int
part1 input =
    let
        check ( ( a, b ), ( c, d ) ) =
            (a >= c && b <= d)
                || (c >= a && d <= b)
    in
    parse input
        |> List.Extra.count check


part2 : String -> Int
part2 input =
    let
        check ( ( a, b ), ( c, d ) ) =
            (b >= c && b <= d)
                || (a >= c && a <= d)
                || (c >= a && c <= b)
                || (d >= a && d <= b)
    in
    parse input
        |> List.Extra.count check
