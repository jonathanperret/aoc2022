module Day11Tests exposing (..)

import AocUtil exposing (..)
import Array exposing (Array)
import Day11Input exposing (input)
import Debug as D
import Expect
import Fuzz
import List as L
import List.Extra as LE
import Regex
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
                            { starting = L.map toNum [ 79, 98 ]
                            , op = Mul 19
                            , divIndex = 8
                            , ifTrue = 2
                            , ifFalse = 3
                            , activity = 0
                            }
            , test "evalMonkey" <|
                \_ ->
                    evalMonkey
                        { starting = L.map toNum [ 79, 98, 23 ]
                        , op = Mul 19
                        , divIndex = 8
                        , ifTrue = 2
                        , ifFalse = 3
                        , activity = 0
                        }
                        |> Expect.equal
                            ( ( 2, [ [ 1, 2, 2, 3, 8, 8, 12, 0, 0 ] ] ), ( 3, [ [ 1, 1, 1, 3, 5, 6, 5, 0, 6 ], [ 0, 2, 2, 0, 3, 3, 9, 0, 22 ] ] ) )
            , test "toNum" <| \_ -> toNum 123 |> Expect.equal [ 1, 0, 3, 4, 2, 6, 4, 9, 8 ]
            , test "numAddInt" <|
                \_ ->
                    numAddInt (toNum 123) 456
                        |> Expect.equal [ 1, 0, 4, 5, 7, 7, 1, 9, 4 ]
            , test "numMulInt" <|
                \_ ->
                    numMulInt (toNum 123) 456
                        |> Expect.equal [ 0, 0, 3, 4, 10, 6, 5, 0, 14 ]
            , test "round" <|
                \_ ->
                    example1
                        |> parseMonkeys
                        |> round
                        |> Expect.equal
                            [ { activity = 2
                              , divIndex = 8
                              , ifFalse = 3
                              , ifTrue = 2
                              , op = Mul 19
                              , starting =
                                    [ [ 0, 0, 0, 4, 5, 8, 9, 3, 14 ]
                                    , [ 1, 2, 1, 1, 5, 6, 3, 14, 2 ]
                                    , [ 1, 0, 1, 4, 4, 3, 13, 5, 12 ]
                                    , [ 0, 2, 0, 3, 3, 2, 12, 4, 11 ]
                                    ]
                              }
                            , { activity = 4
                              , divIndex = 7
                              , ifFalse = 0
                              , ifTrue = 2
                              , op = Add 6
                              , starting =
                                    [ [ 1, 2, 2, 0, 0, 12, 9, 1, 8 ]
                                    , [ 0, 1, 4, 6, 8, 9, 8, 3, 9 ]
                                    , [ 1, 2, 0, 3, 6, 6, 12, 3, 2 ]
                                    , [ 0, 1, 4, 0, 7, 4, 5, 12, 11 ]
                                    , [ 1, 0, 3, 5, 6, 2, 16, 12, 15 ]
                                    , [ 0, 1, 2, 4, 7, 0, 11, 7, 5 ]
                                    ]
                              }
                            , { activity = 3
                              , divIndex = 5
                              , ifFalse = 3
                              , ifTrue = 1
                              , op = Square
                              , starting = []
                              }
                            , { activity = 6
                              , divIndex = 6
                              , ifFalse = 1
                              , ifTrue = 0
                              , op = Add 3
                              , starting = []
                              }
                            ]
            , test "example 20 rounds" <| \_ -> example1 |> part2 20 |> Expect.equal (99 * 103)
            , test "example" <| \_ -> example1 |> part2 10000 |> Expect.equal 2713310158
            , test "input" <| \_ -> input |> part2 10000 |> Expect.equal 39109444654
            ]
        ]


primes =
    [ 2, 3, 5, 7, 11, 13, 17, 19, 23 ]


type alias Num =
    List Int


toNum i =
    primes
        |> L.map (\p -> modBy p i)


type Op
    = Mul Int
    | Add Int
    | Square


type alias Monkey =
    { starting : List Num
    , op : Op
    , divIndex : Int
    , ifTrue : Int
    , ifFalse : Int
    , activity : Int
    }


monkeyRE =
    Regex.fromString """Monkey \\d+:
  Starting items: (.*)
  Operation: new = old ([*+]) (\\d+|old)
  Test: divisible by (\\d+)
    If true: throw to monkey (\\d+)
    If false: throw to monkey (\\d+)"""
        |> fromJust


parseMonkey : String -> Monkey
parseMonkey input =
    case
        Regex.find monkeyRE input
            |> List.concatMap .submatches
            |> List.filterMap identity
            |> List.map (\x -> ( x, String.toInt x ))
    of
        [ ( startingStr, _ ), ( opStr, _ ), ( argStr, argInt ), ( _, Just divInt ), ( _, Just ifTrueInt ), ( _, Just ifFalseInt ) ] ->
            { starting =
                startingStr |> String.split "," |> List.filterMap (String.trim >> String.toInt >> Maybe.map toNum)
            , op =
                case ( opStr, argInt ) of
                    ( "*", Nothing ) ->
                        Square

                    ( "*", Just arg ) ->
                        Mul arg

                    ( "+", Just arg ) ->
                        Add arg

                    _ ->
                        Debug.todo (String.concat [ "bad op ", opStr ])
            , divIndex = divInt |> (\d -> LE.findIndex ((==) d) primes |> fromJust)
            , ifTrue = ifTrueInt
            , ifFalse = ifFalseInt
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
                m =
                    LE.getAt i ms |> fromJust

                ( ( t1, items1 ), ( t2, items2 ) ) =
                    evalMonkey m
            in
            ms
                |> LE.updateAt i (\tm -> { tm | starting = [], activity = tm.activity + (tm.starting |> List.length) })
                |> LE.updateAt t1 (\tm -> { tm | starting = tm.starting ++ items1 })
                |> LE.updateAt t2 (\tm -> { tm | starting = tm.starting ++ items2 })
    in
    List.range 0 (List.length monkeys - 1)
        |> List.foldl applyMonkeyAt monkeys


type alias MonkeyResult =
    ( ( Int, List Num ), ( Int, List Num ) )


numAddInt mods i =
    List.map2 (\m p -> modBy p (m + i)) mods primes


numMulInt mods i =
    List.map2 (\m p -> modBy p (m * i)) mods primes


numSquare mods =
    List.map2 (\m p -> modBy p (m * m)) mods primes


numDivisibleBy : Int -> Num -> Bool
numDivisibleBy index mods =
    LE.getAt index mods |> fromJust |> (==) 0



--numDivisibleBy d n =
--    modBy d (fromNat n) == 0


evalMonkey : Monkey -> MonkeyResult
evalMonkey monkey =
    let
        processItem : Num -> ( Bool, Num )
        processItem item =
            let
                afterOp =
                    case monkey.op of
                        Add n ->
                            numAddInt item n

                        Mul n ->
                            numMulInt item n

                        Square ->
                            numSquare item

                result =
                    numDivisibleBy monkey.divIndex afterOp
            in
            ( result, afterOp )

        processed =
            List.map processItem monkey.starting

        trueItems =
            processed |> List.filter (\( r, v ) -> r) |> List.map Tuple.second

        falseItems =
            processed |> List.filter (\( r, v ) -> not r) |> List.map Tuple.second
    in
    ( ( monkey.ifTrue, trueItems ), ( monkey.ifFalse, falseItems ) )


part2 roundCount input =
    let
        monkeys =
            input |> parseMonkeys

        afterRounds =
            List.range 0 (roundCount - 1)
                |> List.foldl (\_ ms -> round ms) monkeys
                |> Debug.log "after"
    in
    afterRounds
        |> List.map .activity
        |> List.sort
        |> List.reverse
        |> List.take 2
        |> List.foldl (*) 1
