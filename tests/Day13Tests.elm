module Day13Tests exposing (..)

import AocUtil exposing (..)
import Day13Input exposing (input)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set
import String.Extra
import Test exposing (..)
import Tuple
import String as S
import List as L
import Debug as D
import Array exposing (Array)
import Json.Decode as JD
import Basics.Extra exposing (..)
import Day13 exposing (..)

example1 = [
      Msg [Num 1,Num 1,Num 3,Num 1,Num 1]
    , Msg [Num 1,Num 1,Num 5,Num 1,Num 1]
    , Msg [Msg [Num 1],Msg [Num 2,Num 3,Num 4]]
    , Msg [Msg [Num 1],Num 4]
    , Msg [Num 9]
    , Msg [Msg [Num 8,Num 7,Num 6]]
    , Msg [Msg [Num 4,Num 4],Num 4,Num 4]
    , Msg [Msg [Num 4,Num 4],Num 4,Num 4,Num 4]
    , Msg [Num 7,Num 7,Num 7,Num 7]
    , Msg [Num 7,Num 7,Num 7]
    , Msg []
    , Msg [Num 3]
    , Msg [Msg [Msg []]]
    , Msg [Msg []]
    , Msg [Num 1,Msg [Num 2,Msg [Num 3,Msg [Num 4,Msg [Num 5,Num 6,Num 7]]]],Num 8,Num 9]
    , Msg [Num 1,Msg [Num 2,Msg [Num 3,Msg [Num 4,Msg [Num 5,Num 6,Num 0]]]],Num 8,Num 9]
    ]


suite : Test
suite =
    describe "day 13"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> Expect.equal 13
            , test "input" <| \_ -> input |> part1 |> Expect.equal 5292
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 140
            , test "input" <| \_ -> input |> part2 |> Expect.equal 23868
            ]
        ]

findMap2: (a -> b -> Maybe c) -> List a -> List b -> Maybe c
findMap2 f l1 l2 = LE.zip l1 l2 |> LE.findMap (uncurry f)

isInRightOrder: Message -> Message -> Maybe Bool
isInRightOrder m1 m2 =
    (case (m1, m2) of
        (Num a, Msg b) -> isInRightOrder (Msg [m1]) m2
        (Msg a, Num b) -> isInRightOrder m1 (Msg [m2])
        (Num a, Num b) -> if a==b then Nothing else Just (a < b)
        (Msg a, Msg b) ->
            case findMap2 isInRightOrder a b of
                Just r -> Just r
                Nothing ->
                    if L.length a == L.length b then Nothing
                    else Just (L.length a < L.length b))
    --|> Debug.log (Debug.toString m1 ++ " " ++ Debug.toString m2)


part1 input =
    input
    |> LE.groupsOf 2
    |> L.map (\pair -> case pair of
        [m1,m2] -> isInRightOrder m1 m2 |> Maybe.withDefault True
        _ -> Debug.todo "bad pair")
    |> L.indexedMap (\i t -> if t then (i + 1) else 0)
    |> L.foldl (+) 0

compareMsg: Message -> Message -> Order
compareMsg m1 m2 = case isInRightOrder m1 m2 of
    Just True -> LT
    Just False -> GT
    Nothing -> EQ

part2 input =
    input
    |> (L.append [ Msg [Msg [Num 2]], Msg [Msg [Num 6]] ])
    |> L.sortWith compareMsg
    |> L.indexedMap (\i m ->
        case m of
           Msg [Msg [Num x]] -> if x == 2 || x == 6 then (i + 1) else 1
           _ -> 1)
    |> L.foldl (*) 1
