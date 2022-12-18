module Day18Tests exposing (..)

import AocUtil exposing (..)
import Day18Input exposing (..)
import Expect
import Set
import Test exposing (..)
import Day18 exposing (..)

suite : Test
suite =
    describe "day 18"
        [ describe "part 1"
            [ test "countSides" <|
                \_ ->
                    Set.singleton ( -1, 0, 0 )
                        |> countSides
                        |> Expect.equal
                            5
            , test "example0" <| \_ -> "1,1,1\n2,1,1" |> part1 |> Expect.equal 10
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 64
            , test "input" <| \_ -> input |> part1 |> Expect.equal 4548
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 58
            , test "input" <| \_ -> input |> part2 |> Expect.equal 2588
            ]
        ]
