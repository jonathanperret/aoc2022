module Day4 exposing (..)

import Day4Input exposing (input)
import Expect
import Fuzz
import List.Extra
import Set
import Test exposing (..)


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
            [
             test "example" <| \_ -> example1 |> part1 |> Expect.equal 2
            , test "input" <| \_ -> input |> part1 |> Expect.equal 496
            ]
        , describe "part 2"
            [
              test "example" <| \_ -> example1 |> part2 |> Expect.equal 4
            , test "input" <| \_ -> input |> part2 |> Expect.equal 847
            ]
        ]

part1 : String -> Int
part1 input =
    let
        lines =
            String.lines input

        parse line =
            let
                rangesStr = String.split "," line
                rnges = rangesStr |> List.map (String.split "-" >> List.map (String.toInt >> Maybe.withDefault 0))
            in rnges

        pairs = lines |> List.map parse

        check pair = case pair of
            [[a,b],[c,d]] ->
                (a >= c && b <= d)
                || (c >= a && d <= b)
            _ -> False

    in
        List.Extra.count check pairs

part2 : String -> Int
part2 input =
    let
        lines =
            String.lines input

        parse line =
            let
                rangesStr = String.split "," line
                rnges = rangesStr |> List.map (String.split "-" >> List.map (String.toInt >> Maybe.withDefault 0))
            in rnges

        pairs = lines |> List.map parse

        check pair = case pair of
            [[a,b],[c,d]] ->
                (b >= c && b <= d)
                || (a >= c && a <= d)
                || (c >= a && c <= b)
                || (d >= a && d <= b)
            _ -> False

    in
        List.Extra.count check pairs

