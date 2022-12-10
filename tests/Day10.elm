module Day10 exposing (..)

import AocUtil exposing (..)
import Array exposing (Array)
import Day10Input exposing (input)
import Debug as D
import Expect
import Fuzz
import List as L
import List.Extra as LE
import Set
import String as S
import String.Extra
import Test exposing (..)
import Tuple


example1 =
    """noop
addx 3
addx -5"""


example2 =
    """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"""


suite : Test
suite =
    describe "day 10"
        [ describe "part 1"
            [ test "xvalues" <| \_ -> example1 |> xvalues |> Expect.equal [ 1, 1, 1, 4, 4, -1 ]
            , test "example2" <| \_ -> example2 |> part1 |> Expect.equal 13140
            , test "input" <| \_ -> input |> part1 |> Expect.equal 14720
            ]
        , describe "part 2"
            [ test "example2" <|
                \_ ->
                    example2
                        |> part2
                        |> Expect.equal
                            [ "##..##..##..##..##..##..##..##..##..##.."
                            , "###...###...###...###...###...###...###."
                            , "####....####....####....####....####...."
                            , "#####.....#####.....#####.....#####....."
                            , "######......######......######......####"
                            , "#######.......#######.......#######....."
                            ]
            , test "input" <|
                \_ ->
                    input
                        |> part2
                        |> Expect.equal
                            [ "####.####.###..###..###..####.####.####."
                            , "#.......#.#..#.#..#.#..#.#.......#.#...."
                            , "###....#..###..#..#.###..###....#..###.."
                            , "#.....#...#..#.###..#..#.#.....#...#...."
                            , "#....#....#..#.#....#..#.#....#....#...."
                            , "#....####.###..#....###..#....####.#...."
                            ]
            ]
        ]


xvalues : String -> List Int
xvalues input =
    let
        ( finalX, finalPastXs ) =
            String.lines input
                |> L.foldl
                    (\instr ( x, pastXs ) ->
                        case
                            instr
                                |> String.words
                                |> List.tail
                                |> Maybe.andThen List.head
                                |> Maybe.andThen String.toInt
                        of
                            Just val ->
                                ( x + val, x :: x :: pastXs )

                            _ ->
                                ( x, x :: pastXs )
                    )
                    ( 1, [] )
    in
    (finalX :: finalPastXs) |> L.reverse


part1 input =
    xvalues input
        |> L.indexedMap
            (\c x ->
                if (c + 1 - 20 |> modBy 40) == 0 then
                    Just ( c + 1, x )

                else
                    Nothing
            )
        |> L.filterMap identity
        |> L.map (\( c, x ) -> c * x)
        |> L.sum


part2 : String -> List String
part2 input =
    xvalues input
        |> L.indexedMap
            (\c x ->
                let
                    col =
                        modBy 40 c

                    hit =
                        x == col || (x - 1) == col || (x + 1) == col
                in
                if hit then
                    "#"

                else
                    "."
            )
        |> LE.groupsOf 40
        |> L.map (String.join "")
