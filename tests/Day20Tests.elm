module Day20Tests exposing (..)

import AocUtil exposing (..)
import Array exposing (Array)
import Day20Input exposing (input)
import Debug as D
import Dict exposing (Dict)
import Expect
import Fuzz
import List as L
import List.Extra as LE
import Regex
import Set exposing (Set)
import String as S
import String.Extra
import String.Interpolate exposing (interpolate)
import Test exposing (..)
import Tuple


example1 =
    """1
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
            [ test "moveOne 0" <|
                \_ ->
                    [ 0, 1, 2, 3, 4, 5, 6 ]
                        |> moveOne 0 (Value 1)
                        |> zeroFirst
                        |> Expect.equal
                            [ 0, 2, 3, 4, 5, 6, 1 ]
            , test "moveOne 1" <|
                \_ ->
                    [ 1, 0, 2, 3, 4, 5, 6 ]
                        |> moveOne 1 (Value 2)
                        |> zeroFirst
                        |> Expect.equal
                            [ 0, 2, 1, 3, 4, 5, 6 ]
            , test "moveOne 2" <|
                \_ ->
                    [ 0, 2, 1, 3, 4, 5, 6 ]
                        |> moveOne 2 (Value -3)
                        |> zeroFirst
                        |> Expect.equal
                            [ 0, 1, 3, 4, 2, 5, 6 ]
            , test "moveOne 3" <|
                \_ ->
                    [ 0, 1, 3, 4, 2, 5, 6 ]
                        |> moveOne 3 (Value 3)
                        |> zeroFirst
                        |> Expect.equal
                            [ 0, 1, 4, 2, 5, 3, 6 ]
            , test "moveOne 4" <|
                \_ ->
                    [ 0, 1, 4, 2, 5, 3, 6 ]
                        |> moveOne 4 (Value -2)
                        |> zeroFirst
                        |> Expect.equal
                            [ 0, 1, 2, 5, 3, 6, 4 ]
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 3
            , test "input" <| \_ -> input |> part1 |> Expect.equal 988
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 1623178306
            , test "input" <| \_ -> input |> part2 |> Expect.equal 7768531372516
            ]
        ]


type Value
    = Value Int


zeroFirst indices =
    case LE.splitWhen (\i -> i == 0) indices of
        Just ( left, right ) ->
            right ++ left

        _ ->
            D.todo "no zero"


insertAt index value list =
    let
        ( left, right ) =
            LE.splitAt index list
    in
    left ++ [ value ] ++ right


moveOne : Int -> Value -> List Int -> List Int
moveOne initialIndex (Value value) indexes =
    let
        ( left, right ) =
            case LE.splitWhen (\i -> i == initialIndex) indexes of
                Just ( l, _ :: r ) ->
                    ( l, r )

                _ ->
                    D.todo "oops"

        newIndex =
            modBy (L.length indexes - 1) value

        without =
            right ++ left
    in
    insertAt newIndex initialIndex without


mix : List Value -> List Int -> List Int
mix values indices =
    L.foldl
        (\( i, v ) l -> moveOne i v l)
        indices
        (L.indexedMap (\i v -> ( i, v )) values)


nthAfter0 : List Value -> Int -> List Int -> Value
nthAfter0 original delta mixed =
    let
        initialZeroIndex =
            LE.findIndex (\(Value x) -> x == 0) original |> fromJust

        newZeroIndex =
            LE.findIndex (\x -> x == initialZeroIndex) mixed |> fromJust

        targetIndex =
            modBy (L.length original) (newZeroIndex + delta)

    in
    LE.getAt (LE.getAt targetIndex mixed |> fromJust) original |> fromJust


sumCoordinates values indices =
    let
        coordinates =
            [ 1000, 2000, 3000 ]
                |> L.map (\delta -> nthAfter0 values delta indices)
    in
    coordinates |> L.map (\(Value x) -> x) |> L.sum


part1 input =
    let
        values =
            input
                |> S.lines
                |> L.filterMap S.toInt
                |> L.map Value

        indices =
            L.range 0 (L.length values - 1)

        mixedIndices =
            mix values indices
                |> D.log "mixed"
    in
    sumCoordinates values mixedIndices


part2 input =
    let
        values =
            input
                |> S.lines
                |> L.filterMap S.toInt
                |> L.map ((*) 811589153)
                |> L.map Value

        indices =
            L.range 0 (L.length values - 1)

        mixedIndices =
            L.range 0 9
                |> L.foldl (\_ -> mix values) indices
    in
    sumCoordinates values mixedIndices
