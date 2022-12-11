module Day11Tests exposing (..)

import AocUtil exposing (..)
import Array exposing (Array)
import Day11Input exposing (input)
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
    """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"""


suite : Test
suite =
    describe "day 11"
        [ describe "part 1"
            [ test "parseMonkey" <|
                \_ ->
                    """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3"""
                        |> parseMonkey
                        |> Expect.equal
                            { index = 0
                            , starting = [ 79, 98 ]
                            , op = Mul 19
                            , div = 23
                            , ifTrue = 2
                            , ifFalse = 3
                            , activity = 0
                            }
            , test "evalMonkey" <|
                \_ ->
                    evalMonkey
                        { index = 0
                        , starting = [ 79, 98, 23 * 7 * 3 ]
                        , op = Mul 19
                        , div = 23
                        , ifTrue = 2
                        , ifFalse = 3
                        , activity = 0
                        }
                        |> Expect.equal
                            ( ( 2, [ 23 * 7 * 19 ] ), ( 3, [ 500, 620 ] ) )
            , test "round" <|
                \_ ->
                    example1
                        |> parseMonkeys
                        |> round
                        |> Expect.equal
                            [ { activity = 2, div = 23, ifFalse = 3, ifTrue = 2, index = 0, op = Mul 19, starting = [ 20, 23, 27, 26 ] }
                            , { activity = 4, div = 19, ifFalse = 0, ifTrue = 2, index = 1, op = Add 6, starting = [2080, 25, 167, 207, 401, 1046] }
                            , { activity = 3, div = 13, ifFalse = 3, ifTrue = 1, index = 2, op = Square, starting = [] }
                            , { activity = 5, div = 17, ifFalse = 1, ifTrue = 0, index = 3, op = Add 3, starting = [] }
                            ]
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 10605
            , test "input" <| \_ -> input |> part1 |> Expect.equal 120756
            ]
        , skip <|
            describe "part 2"
                [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 0
                , test "input" <| \_ -> input |> part2 |> Expect.equal 0
                ]
        ]


type Op
    = Mul Int
    | Add Int
    | Square


type alias Monkey =
    { index : Int
    , starting : List Int
    , op : Op
    , div : Int
    , ifTrue : Int
    , ifFalse : Int
    , activity : Int
    }


parseMonkey : String -> Monkey
parseMonkey input =
    case input |> String.lines |> List.map (String.split ":") of
        [ [ monkeyStr, "" ], [ "  Starting items", startingStr ], [ "  Operation", opStr ], [ "  Test", testStr ], [ "    If true", ifTrueStr ], [ "    If false", ifFalseStr ] ] ->
            { index =
                case String.words monkeyStr of
                    [ "Monkey", indexStr ] ->
                        indexStr |> String.toInt |> fromJust

                    _ ->
                        Debug.todo (String.concat [ "bad monkeyStr ", monkeyStr ])
            , starting = startingStr |> String.split "," |> List.map (String.trim >> String.toInt >> fromJust)
            , op =
                case opStr |> String.words of
                    [ "new", "=", "old", "*", "old" ] ->
                        Square

                    [ "new", "=", "old", "*", mult ] ->
                        Mul (mult |> String.toInt |> fromJust)

                    [ "new", "=", "old", "+", add ] ->
                        Add (add |> String.toInt |> fromJust)

                    _ ->
                        Debug.todo (String.concat [ "bad op ", opStr ])
            , div =
                case testStr |> String.words of
                    [ "divisible", "by", divisor ] ->
                        divisor |> String.toInt |> fromJust

                    _ ->
                        Debug.todo (String.concat [ "bad test ", testStr ])
            , ifTrue =
                case ifTrueStr |> String.words of
                    [ "throw", "to", "monkey", target ] ->
                        target |> String.toInt |> fromJust

                    _ ->
                        Debug.todo (String.concat [ "bad ifTrue ", ifTrueStr ])
            , ifFalse =
                case ifFalseStr |> String.words of
                    [ "throw", "to", "monkey", target ] ->
                        target |> String.toInt |> fromJust

                    _ ->
                        Debug.todo (String.concat [ "bad ifFalse ", ifFalseStr ])
            , activity = 0
            }

        _ ->
            Debug.todo (String.concat [ "bad monkey ", input ])


parseMonkeys : String -> List Monkey
parseMonkeys input =
    input
        |> String.split "\n\n"
        |> List.map parseMonkey


round : List Monkey -> List Monkey
round monkeys =
    let
        applyMonkeyAt : Int -> List Monkey -> List Monkey
        applyMonkeyAt i ms =
            let
                m = LE.getAt i ms |> fromJust
                ( ( t1, items1 ), ( t2, items2 ) ) =
                    evalMonkey m
            in
            ms
                |> LE.updateAt m.index (\tm -> { tm | starting = [], activity = tm.activity + (tm.starting |> List.length) })
                |> LE.updateAt t1 (\tm -> { tm | starting = tm.starting ++ items1 })
                |> LE.updateAt t2 (\tm -> { tm | starting = tm.starting ++ items2 })
    in
    List.range 0 (List.length monkeys - 1)
        |> List.foldl applyMonkeyAt monkeys


type alias MonkeyResult =
    ( ( Int, List Int ), ( Int, List Int ) )


evalMonkey : Monkey -> MonkeyResult
evalMonkey monkey =
    let
        processItem item =
            let
                afterOp =
                    case monkey.op of
                        Add n ->
                            item + n

                        Mul n ->
                            item * n

                        Square ->
                            item * item

                afterBored =
                    afterOp // 3

                result =
                    modBy monkey.div afterBored == 0
            in
            ( result, afterBored )

        processed =
            List.map processItem monkey.starting

        trueItems =
            processed |> List.filter (\( r, v ) -> r) |> List.map Tuple.second

        falseItems =
            processed |> List.filter (\( r, v ) -> not r) |> List.map Tuple.second
    in
    ( ( monkey.ifTrue, trueItems ), ( monkey.ifFalse, falseItems ) )


part1 input =
    let
        monkeys =
            input |> parseMonkeys

        after20rounds =
            List.range 0 19
            |> List.foldl (\_ ms -> round ms) monkeys
            |> Debug.log "after"
    in
    after20rounds
    |> List.map .activity
    |> List.sort
    |> List.reverse
    |> List.take 2
    |> List.foldl (*) 1


part2 : String -> Int
part2 input =
    0
