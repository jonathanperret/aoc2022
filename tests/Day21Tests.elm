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
            , test "input" <| \_ -> input |> part1 |> Integer.toString |> Expect.equal "82225382988628"
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Integer.toString |> Expect.equal "301"
            , test "input" <| \_ -> input |> part2 |> Integer.toString |> Expect.equal "3429411069028"
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


evalNonHuman: Dict String Monkey -> String -> Maybe Integer
evalNonHuman monkeys name =
    if name == "humn" then
        Nothing
    else
    let
        targetMonkey = case Dict.get name monkeys of
            Just m -> m
            _ -> D.todo ("unknown monkey" ++ name)

        result = case targetMonkey.op of
            Literal n -> Just n
            Add arg1 arg2 -> Maybe.map2 Integer.add (evalNonHuman monkeys arg1) (evalNonHuman monkeys arg2)
            Sub arg1 arg2 -> Maybe.map2 Integer.sub (evalNonHuman monkeys arg1) (evalNonHuman monkeys arg2)
            Mul arg1 arg2 -> Maybe.map2 Integer.mul (evalNonHuman monkeys arg1) (evalNonHuman monkeys arg2)
            Div arg1 arg2 ->
                let
                    val1 = (evalNonHuman monkeys arg1)
                    val2 = (evalNonHuman monkeys arg2)
                in
                    case Maybe.map2 Integer.div val1 val2 of
                        Just x -> x
                        _ -> Nothing
                         -- D.todo (interpolate "division by zero at {0}: {1} / {2}" [ name, D.toString <| Maybe.map Integer.toString val1, D.toString <| Maybe.map Integer.toString val2 ])
    in
    result |> D.log ("eval "++name)


solveTarget: Dict String Monkey -> String -> Integer -> Integer
solveTarget monkeys name target =
    if name == "humn" then
        target
    else
    let
        targetMonkey = case Dict.get name monkeys of
            Just m -> m
            _ -> D.todo ("unknown monkey" ++ name)

        result = case targetMonkey.op of
            Literal _ -> D.todo "can't solve literal"
            Add arg1 arg2 ->
                case (evalNonHuman monkeys arg1, evalNonHuman monkeys arg2) of
                    (Nothing, Just val2) -> solveTarget monkeys arg1 (Integer.sub target val2)
                    (Just val1, Nothing) -> solveTarget monkeys arg2 (Integer.sub target val1)
                    _ -> D.todo "one of the branches needs to have a human"
            Sub arg1 arg2 ->
                case (evalNonHuman monkeys arg1, evalNonHuman monkeys arg2) of
                    (Nothing, Just val2) -> solveTarget monkeys arg1 (Integer.add target val2)
                    (Just val1, Nothing) -> solveTarget monkeys arg2 (Integer.sub val1 target)
                    _ -> D.todo "one of the branches needs to have a human"
            Mul arg1 arg2 ->
                case (evalNonHuman monkeys arg1, evalNonHuman monkeys arg2) of
                    (Nothing, Just val2) -> solveTarget monkeys arg1 (Integer.div target val2 |> fromJust)
                    (Just val1, Nothing) -> solveTarget monkeys arg2 (Integer.div target val1 |> fromJust)
                    _ -> D.todo "one of the branches needs to have a human"
            Div arg1 arg2 ->
                case (evalNonHuman monkeys arg1, evalNonHuman monkeys arg2) of
                    (Nothing, Just val2) -> solveTarget monkeys arg1 (Integer.mul target val2)
                    (Just val1, Nothing) -> solveTarget monkeys arg2 (Integer.div val1 target |> fromJust)
                    _ -> D.todo "one of the branches needs to have a human"

    in
    result

solve: Dict String Monkey -> String -> Integer
solve monkeys name =
    let
        targetMonkey = case Dict.get name monkeys of
            Just m -> m
            _ -> D.todo ("unknown monkey" ++ name)

        result = case targetMonkey.op of
            Add arg1 arg2 ->
                case (evalNonHuman monkeys arg1, evalNonHuman monkeys arg2) of
                    (Nothing, Just val2) -> solveTarget monkeys arg1 val2
                    (Just val1, Nothing) -> solveTarget monkeys arg2 val1
                    _ -> D.todo "one of the branches needs to have a human"
            _ -> D.todo "can't solve this"
    in
    result

part1 input =
    let
        monkeys =
            input
            |> parse
    in
    eval monkeys "root"

part2 input =
    let
        monkeys =
            input
            |> parse
    in
    solve monkeys "root"

