module Day2 exposing (..)

import Day2Input exposing (input)
import Expect
import Fuzz
import Test exposing (..)


example1 =
    """A Y
B X
C Z"""


suite : Test
suite =
    describe "day 2"
        [ describe "part 1"
            [ test "scoreRound" <|
                \_ -> scoreRound "A Y" |> Expect.equal 8
            , test "example" <|
                \_ ->
                    example1
                        |> scoreRounds
                        |> Expect.equal 15
            , test "input" <|
                \_ ->
                    input
                        |> scoreRounds
                        |> Expect.equal 11063
            ]
        , describe "part 2"
            [ test "scoreRoundPart2" <|
                \_ -> scoreRoundPart2 "A Y" |> Expect.equal 4
            , test "example" <|
                \_ ->
                    example1
                        |> scoreRoundsPart2
                        |> Expect.equal 12
            , test "input" <|
                \_ ->
                    input
                        |> scoreRoundsPart2
                        |> Expect.equal 10349
            ]
        ]


scoreRound : String -> Int
scoreRound line =
    let
        score =
            case line of
                "A X" ->
                    1 + 3

                "A Y" ->
                    2 + 6

                "A Z" ->
                    3 + 0

                "B X" ->
                    1 + 0

                "B Y" ->
                    2 + 3

                "B Z" ->
                    3 + 6

                "C X" ->
                    1 + 6

                "C Y" ->
                    2 + 0

                "C Z" ->
                    3 + 3

                _ ->
                    Debug.todo (String.concat [ "unknown match ", line ])
    in
    score


scoreRoundPart2 : String -> Int
scoreRoundPart2 line =
    let
        score =
            case line of
                "A X" ->
                    scoreRound "A Z"

                "A Y" ->
                    scoreRound "A X"

                "A Z" ->
                    scoreRound "A Y"

                "B X" ->
                    scoreRound "B X"

                "B Y" ->
                    scoreRound "B Y"

                "B Z" ->
                    scoreRound "B Z"

                "C X" ->
                    scoreRound "C Y"

                "C Y" ->
                    scoreRound "C Z"

                "C Z" ->
                    scoreRound "C X"

                _ ->
                    Debug.todo (String.concat [ "unknown part2 match ", line ])
    in
    score


scoreRounds : String -> Int
scoreRounds input =
    let
        rounds =
            String.lines input
    in
    List.map scoreRound rounds
        |> List.sum


scoreRoundsPart2 : String -> Int
scoreRoundsPart2 input =
    let
        rounds =
            String.lines input
    in
    List.map scoreRoundPart2 rounds
        |> List.sum
