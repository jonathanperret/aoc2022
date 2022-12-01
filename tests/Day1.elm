module Day1 exposing (..)
import Day1Input exposing (input)

import Expect
import Fuzz
import Test exposing (..)

example1 = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""

suite : Test
suite =
    describe "day 01"
        [ describe "part 1"
            [ test "example" <|
                \_ ->
                    example1
                        |> part1
                        |> Expect.equal 24000
            , test "input" <|
                \_ ->
                    input
                        |> part1
                        |> Expect.equal 69501
            ]
        , describe "part 2"
            [ test "example" <|
                \_ ->
                    example1
                        |> part2
                        |> Expect.equal 45000
            , test "input" <|
                \_ ->
                    input
                        |> part2
                        |> Expect.equal 202346
            ]
        ]


part1 : String -> Int
part1 str =
    let
        lines =
            String.lines str

        step =
            \line ( grps, current ) ->
                if line == "" then
                    ( grps ++ [ current ], [] )

                else
                    ( grps, current ++ [ line ] )

        state =
            ( [], [] )

        groups =
            List.foldl step state lines
                |> (\( grps, last ) -> grps ++ [ last ])

        sums =
            List.map (\grp -> List.map String.toInt grp |> List.map (\m -> Maybe.withDefault 0 m) |> List.sum) groups

        result =
            List.maximum sums
                |> Maybe.withDefault 0
    in
    result


part2 : String -> Int
part2 str =
    let
        lines =
            String.lines str

        step =
            \line ( grps, current ) ->
                if line == "" then
                    ( grps ++ [ current ], [] )

                else
                    ( grps, current ++ [ line ] )

        state =
            ( [], [] )

        groups =
            List.foldl step state lines
                |> (\( grps, last ) -> grps ++ [ last ])

        sums =
            List.map (\grp -> List.map String.toInt grp |> List.map (\m -> Maybe.withDefault 0 m) |> List.sum) groups

        tops = List.sort sums
            |> List.reverse
            |> List.take 3
            |> Debug.log "tops"

        result =
            List.sum tops
    in
    result
