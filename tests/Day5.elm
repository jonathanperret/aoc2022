module Day5 exposing (..)

import AocUtil exposing (..)
import Day5Input exposing (input)
import Expect
import Fuzz
import List.Extra
import String.Extra
import Set
import Test exposing (..)
import Tuple


example1 =
    """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""


suite : Test
suite =
    describe "day 5"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> Expect.equal "CMZ"
            , test "input" <| \_ -> input |> part1 |> Expect.equal "FRDSQRRCD"
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal "MCD"
            , test "input" <| \_ -> input |> part2 |> Expect.equal "HRFTQVWNN"
            ]
        ]

type alias Crates = List(List Char)
type alias Instr = (Int,Int,Int)


parse : String -> (Crates, List Instr)
parse input =
    let
        lines = String.lines input
        (cratesLines, instrLines) =
            lines
            |> List.Extra.splitWhen (\l -> l =="")
            |> fromJust
            |> \(a,b) -> (a, List.drop 1 b)

        crates = cratesLines
            |> List.map (\line -> line |> String.Extra.break 4 |> List.map (String.toList >> List.Extra.getAt 1 >> fromJust))
            |> List.reverse
            |> List.drop 1
            |> List.Extra.transpose
            |> List.map (List.Extra.dropWhileRight (\c -> c == ' '))
            |> List.map List.reverse

        instrs =
            instrLines
            |> List.map String.words
            |> List.map (\words -> (words |> List.Extra.getAt 1 |> Maybe.andThen String.toInt |> fromJust,
                                    (words |> List.Extra.getAt 3 |> Maybe.andThen String.toInt |> fromJust) - 1,
                                    (words |> List.Extra.getAt 5 |> Maybe.andThen String.toInt |> fromJust) - 1))

    in (crates, instrs)

apply : Instr -> Crates -> Crates
apply (count, from, to) crates =
    let
        crates2 = crates
            |> List.Extra.updateAt from (\l -> List.drop 1 l)
            |> List.Extra.updateAt to
                (\l -> (List.Extra.getAt from crates |> Maybe.andThen List.head |> fromJust) :: l)
    in
    case count of
        1 -> crates2
        _ -> apply (count-1, from, to) crates2

part1 : String -> String
part1 input =
    let
        (crates, instrs) = parse input
        final = List.foldl apply crates instrs
    in
    final |> List.map (\l -> List.head l |> fromJust) |> String.fromList

apply2 : Instr -> Crates -> Crates
apply2 (count, from, to) crates =
    let
        crates2 = crates
            |> List.Extra.updateAt from (\l -> List.drop count l)
            |> List.Extra.updateAt to
                (\l -> (List.Extra.getAt from crates |> fromJust |> List.take count) ++ l)
    in
    crates2

part2 : String -> String
part2 input =
    let
        (crates, instrs) = parse input
        final = List.foldl apply2 crates instrs
    in
    final |> List.map (\l -> List.head l |> fromJust) |> String.fromList
