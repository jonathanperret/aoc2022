module Day5Tests exposing (..)

import AocUtil exposing (..)
import Day5Input exposing (input)
import Expect
import Fuzz
import List.Extra
import Set
import String.Extra
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


type alias Stack = List Char

type alias Stacks =
    List Stack


type alias Instr =
    ( Int, Int, Int )


parse : String -> ( Stacks, List Instr )
parse input =
    let
        lines =
            String.lines input

        ( cratesLines, instrLines ) =
            lines
                |> List.Extra.splitWhen (\l -> l == "")
                |> fromJust

        crates =
            cratesLines
                |> List.map (String.toList >> indexFilter (modBy 4 >> (==) 1))
                |> List.reverse
                |> List.drop 1
                |> List.Extra.transpose
                |> List.map (List.reverse >> List.Extra.dropWhile (\c -> c == ' '))

        parseInstr line =
            case String.words line |> List.filterMap String.toInt of
                [ c, f, t ] ->
                    ( c, f - 1, t - 1 )

                _ ->
                    Debug.todo line

        instrs =
            instrLines |> List.drop 1 |> List.map parseInstr
    in
    ( crates, instrs )


applyWith : (Stack -> Stack) -> Instr -> Stacks -> Stacks
applyWith transformer ( count, from, to ) crates =
    let
        moving = crates
            |> List.Extra.getAt from |> fromJust
            |> List.take count
            |> transformer
    in
    crates
        |> List.Extra.updateAt from (List.drop count)
        |> List.Extra.updateAt to ((++) moving)


apply1 : Instr -> Stacks -> Stacks
apply1 = applyWith List.reverse


topCrates : Stacks -> String
topCrates final =
    final |> List.map (List.head >> fromJust) |> String.fromList


part1 : String -> String
part1 input =
    let
        ( crates, instrs ) =
            parse input
    in
    List.foldl apply1 crates instrs
        |> topCrates


apply2 : Instr -> Stacks -> Stacks
apply2 = applyWith identity


part2 : String -> String
part2 input =
    let
        ( crates, instrs ) =
            parse input
    in
    List.foldl apply2 crates instrs
        |> topCrates
