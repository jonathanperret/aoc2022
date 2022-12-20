module Day20Tests exposing (..)

import AocUtil exposing (..)
import Day20Input exposing (input)
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


example1 = """1
2
-3
3
-2
0
4"""


suite : Test
suite =
    describe "day 20"
        [ describe "part 1"
            [ test "moveOne 0" <| \_ ->
                [(0,1),(1,2),(2,-3),(3,3),(4,-2),(5,0),(6,4)] |> moveOne 0 |> Expect.equal
                [(1,2),(0,1),(2,-3),(3,3),(4,-2),(5,0),(6,4)]
            , test "moveOne 1" <| \_ ->
                [(1,2),(0,1),(2,-3),(3,3),(4,-2),(5,0),(6,4)] |> moveOne 1 |> Expect.equal
                [(0,1),(2,-3),(1,2),(3,3),(4,-2),(5,0),(6,4)]
            , test "moveOne 2" <| \_ ->
                [(0,1),(2,-3),(1,2),(3,3),(4,-2),(5,0),(6,4)] |> moveOne 2 |> Expect.equal
                [(0,1),(1,2),(3,3),(4,-2),(2,-3),(5,0),(6,4)]
            , test "moveOne 3" <| \_ ->
                [(0,1),(1,2),(3,3),(4,-2),(2,-3),(5,0),(6,4)] |> moveOne 3 |> Expect.equal
                [(0,1),(1,2),(4,-2),(2,-3),(5,0),(3,3),(6,4)]
            , test "moveOne 4" <| \_ ->
                [(0,1),(1,2),(4,-2),(2,-3),(5,0),(3,3),(6,4)] |> moveOne 4 |> Expect.equal
                [(4,-2),(0,1),(1,2),(2,-3),(5,0),(3,3),(6,4)]
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 3
            , test "input" <| \_ -> input |> part1 |> Expect.equal 988
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 1623178306
            , test "input" <| \_ -> input |> part2 |> Expect.equal 0
            ]
        ]

insertAt index value list =
    let
        (left, right) = LE.splitAt index list
    in
    left ++ [ value ] ++ right

moveOne initialIndex nums =
    let
        currentIndex = LE.findIndex (\(i, n) -> i == initialIndex) nums |> fromJust
        value = LE.getAt currentIndex nums |> fromJust |> Tuple.second

        newIndex = modBy (L.length nums - 1) (currentIndex + value + L.length nums - 1)

        _= (currentIndex, value, newIndex)
            --|> D.log ((initialIndex|>D.toString) ++ " (currentIndex, value, newIndex)")

        without = LE.removeAt currentIndex nums

        final = insertAt newIndex (initialIndex, value) without
        --_ = final |> L.map Tuple.second |> (D.log ("mixed " ++ D.toString initialIndex ++ "(" ++ D.toString value ++ ")"))
    in
    final

nthAfter0 delta indexed =
    let
        zeroIndex = LE.findIndex (\(i, n) -> n == 0) indexed |> fromJust
        targetIndex = modBy (L.length indexed) (zeroIndex + delta)
        (_, targetValue) = LE.getAt targetIndex indexed |> fromJust
    in
    targetValue

part1 input =
    let
        numbers =
            input
            |> S.lines
            |> L.filterMap S.toInt

        indexed = L.indexedMap (\i n -> (i, n)) numbers
            -- |>D.log "indexed"

        mixedList = mix indexed

        coordinates = [1000,2000,3000]
            |> L.map (\i -> nthAfter0 i mixedList)

    in
    coordinates |> L.sum

mix indexed =
    L.range 0 (L.length indexed - 1)
        |> L.foldl (\i l -> moveOne i l)
        indexed

part2 input =
    let
        numbers =
            input
            |> S.lines
            |> L.filterMap S.toInt
            |> L.map ((*) 811589153)

        indexed = L.indexedMap (\i n -> (i, n)) numbers
            -- |>D.log "indexed"

        mixedList = L.range 0 9
            |> L.foldl (\_ l -> mix l)
            indexed

        coordinates = [1000,2000,3000]
            |> L.map (\i -> nthAfter0 i mixedList)

    in
    coordinates |> L.sum

