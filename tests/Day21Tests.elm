module Day21Tests exposing (..)

import AocUtil exposing (..)
import Day21Input exposing (input)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set exposing (Set)
import Dict exposing (Dict)
import String.Extra
import Test exposing (..)
import Tuple
import String as S
import List as L
import Debug as D
import Array exposing (Array)
import Regex
import String.Interpolate exposing (interpolate)
import Integer exposing (Integer)

example1 = """root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"""


suite : Test
suite =
    describe "day 21"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> Integer.toString |> Expect.equal "152"
            , test "input" <| \_ -> input |> part1 |> Integer.toString |> Expect.equal "0"
            ]
        , skip <| describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 0
            , skip <| test "input" <| \_ -> input |> part2 |> Expect.equal 0
            ]
        ]

type alias Monkey = { name: String, op: Op }

type Op =
    Literal Integer
    | Add String String
    | Sub String String
    | Mul String String
    | Div String String

parseLine: String -> Monkey
parseLine line =
    case Regex.find (Regex.fromString "[^ :]+" |> Maybe.withDefault Regex.never) line
        |> List.map .match of
        [ dest, litStr ] -> { name=dest, op= Literal (Integer.fromString litStr|>fromJust) }
        [ dest, arg1, opStr, arg2 ] ->
            { name=dest, op =
                case opStr of
                    "+" -> Add arg1 arg2
                    "-" -> Sub arg1 arg2
                    "*" -> Mul arg1 arg2
                    "/" -> Div arg1 arg2
                    _ -> D.todo ("bad op " ++ opStr)
              }

        _ -> D.todo ("bad line " ++ line)

parse: String -> Dict String Monkey
parse input =
    input
    |> S.lines
    |> L.map parseLine
    |> L.map (\m -> (m.name, m))
    |> Dict.fromList


eval: Dict String Monkey -> String -> Integer
eval monkeys name =
    let
        targetMonkey = case Dict.get name monkeys of
            Just m -> m
            _ -> D.todo ("unknown monkey" ++ name)
    in
    case targetMonkey.op of
        Literal n -> n
        Add arg1 arg2 -> Integer.add (eval monkeys arg1) (eval monkeys arg2)
        Sub arg1 arg2 -> Integer.sub (eval monkeys arg1) (eval monkeys arg2)
        Mul arg1 arg2 -> Integer.mul (eval monkeys arg1) (eval monkeys arg2)
        Div arg1 arg2 -> case Integer.div (eval monkeys arg1) (eval monkeys arg2) of
            Just x -> x
            _ -> D.todo ("division by zero at " ++ name)


part1 input =
    let
        monkeys =
            input
            |> parse
    in
    eval monkeys "root"

part2 input = 0
