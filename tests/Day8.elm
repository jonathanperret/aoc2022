module Day8 exposing (..)

import AocUtil exposing (..)
import Day8Input exposing (input)
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


example1 =
    """30373
25512
65332
33549
35390"""


suite : Test
suite =
    describe "day 8"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> Expect.equal 21
            , test "input" <| \_ -> input |> part1 |> Expect.equal 1870
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 8
            , fuzzWith {runs=200,distribution=Test.noDistribution} Fuzz.int "input" <| \_ -> input |> part2 |> Expect.equal 517440
            ]
        ]


part1 input =
    let
        trees =
            input
            |> S.lines
            |> L.map (S.toList >> L.map (\c -> Char.toCode c - 48))

        visiblesLeft : Int -> List Int -> List (Int, Int)
        visiblesLeft rowIndex row =
            row
            |> L.foldl (\h (colIndex, found, max) ->
                    if h > max then (colIndex + 1, (rowIndex, colIndex) :: found, h)
                    else (colIndex + 1, found, max)
                ) (0, [], -1)
            |> \(_,found,_) -> found

        visiblesRight : Int ->  List Int ->List (Int, Int)
        visiblesRight rowIndex row =
            row
            |> L.foldr (\h (colIndex, found, max) ->
                    if h > max then (colIndex - 1, (rowIndex, colIndex) :: found, h)
                    else (colIndex - 1, found, max)
                ) (List.length row - 1, [], -1)
            |> \(_,found,_) -> found

        visiblesTop: List (Int, Int)
        visiblesTop =
            trees
            |> LE.transpose
            |> L.indexedMap visiblesLeft
            |> L.concatMap (List.map (\(r,c) -> (c,r)))

        visiblesBottom: List (Int, Int)
        visiblesBottom =
            trees
            |> LE.transpose
            |> L.indexedMap visiblesRight
            |> L.concatMap (List.map (\(r,c) -> (c,r)))

        allVisibles : List (Int, Int)
        allVisibles =
            (L.indexedMap visiblesLeft trees |> L.concat)
            ++ (L.indexedMap visiblesRight trees |> L.concat)
            ++ (visiblesTop)
            ++ (visiblesBottom)

        result =
            allVisibles
            |> LE.unique
            |> L.length
    in
    result

leftScores : List Int -> List Int
leftScores row =
    let
        scoreFor0 : Array Int
        scoreFor0 = Array.repeat 10 0

        iter: Array Int -> Int -> (Array Int, Int)
        iter sf item =
            let
                score = sf |> Array.get item |> fromJust
                sf2 =
                    Array.indexedMap (\i s->
                        if i<=item then 1 else s+1
                    ) sf
            in
            (sf2, score)
    in
    LE.mapAccuml iter scoreFor0 row
    |> Tuple.second

part2: String -> Int
part2 input =
    let
        trees =
            input
            |> S.lines
            |> L.map (S.split "" >> L.map (S.toInt >> fromJust))

        scoresRight =
            trees
            |> L.map (L.reverse >> leftScores >> L.reverse)
            |> L.concat

        scoresLeft =
            trees
            |> L.map leftScores
            |> L.concat

        scoresTopBottom =
            trees
            |> LE.transpose
            |> L.map (\col ->
                L.map2 (*)
                    (leftScores col)
                    (col |> L.reverse |> leftScores |> L.reverse)
                )
            |> LE.transpose
            |> L.concat

        scores =
            List.map3 (\left topbottom right -> left * topbottom * right)
                scoresLeft scoresTopBottom scoresRight

    in
    scores |> L.maximum |> fromJust

