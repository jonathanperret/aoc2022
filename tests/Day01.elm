module Day01 exposing (..)

import Day01Input
import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "day 01"
        [ describe "part 1"
            [ test "1122" <|
                \_ ->
                    "1122"
                        |> part1
                        |> Expect.equal 3
            , test "91212129" <|
                \_ ->
                    "91212129"
                        |> part1
                        |> Expect.equal 9
            , test "input" <|
                \_ ->
                    Day01Input.input
                        |> part1
                        |> Expect.equal 1150
            ]
        , describe "part 2"
            [ test "ex. 1" <|
                \_ ->
                    "1212"
                        |> part2
                        |> Expect.equal 6
            , test "ex. 2" <|
                \_ ->
                    "1221"
                        |> part2
                        |> Expect.equal 0
            , test "input" <|
                \_ ->
                    Day01Input.input
                        |> part2
                        |> Expect.equal 1064
            ]
        ]


part1 : String -> Int
part1 s =
    let
        l =
            String.toList s

        ll =
            (List.drop 1 l) ++ l

        values =
            List.map2
                (\a b ->
                    if a == b then
                        Char.toCode a - 48

                    else
                        0
                )
                l
                ll

        result =
            List.sum values
    in
    result


part2 : String -> Int
part2 s =
    let
        l =
            String.toList s

        ll =
            (List.drop ((List.length l)//2) l) ++ l

        values =
            List.map2
                (\a b ->
                    if a == b then
                        Char.toCode a - 48

                    else
                        0
                )
                l
                ll

        result =
            List.sum values
    in
    result
