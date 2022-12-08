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
            , test "input" <| \_ -> input |> part2 |> Expect.equal 517440
            --, fuzz Fuzz.int "bench" <| \_ -> input |> part2 |> Expect.equal 517440
            , test "extract" <| \_ ->
                Array.fromList [Array.fromList [1,2], Array.fromList [3,4]]
                |> extract (0,0) (1,0)
                |> Expect.equal [1,3]
            , test "leftScores" <| \_ ->
                leftScores [3,0,3,7,3]
                |> Expect.equal [0,1,2,3,1]
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

extract : (Int, Int) -> (Int, Int) -> Array (Array a) -> List a
extract (r0, c0) (dr, dc) m =
    let
        loop (r, c) =
            case Array.get r m |> Maybe.andThen (Array.get c) of
                Nothing -> []
                Just t -> t :: loop (r+dr, c+dc)
    in
    loop (r0, c0)

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
            |> L.map (S.split "" >> L.map (S.toInt >> fromJust) >> Array.fromList)
            |> Array.fromList

        rowCount = Array.length trees
        colCount = Array.get 0 trees |> fromJust |> Array.length

        rowLeftScores = L.range 0 (rowCount - 1)
            |> L.map (\r -> (extract (r,0) (0,1) trees) |> leftScores)

        rowRightScores = L.range 0 (rowCount - 1)
            |> L.map (\r ->
                extract (r,colCount-1) (0,-1) trees
                |> leftScores
                |> L.reverse
                )

        rowTopScores = L.range 0 (colCount - 1)
            |> L.map (\c ->
                extract (0,c) (1,0) trees
                |> leftScores
                )
            |> LE.transpose

        rowBottomScores = L.range 0 (colCount - 1)
            |> L.map (\c ->
                extract (rowCount-1,c) (-1,0) trees
                |> leftScores
                |> L.reverse
                )
            |> LE.transpose

        scores =
            [rowLeftScores,rowRightScores,rowTopScores,rowBottomScores]
            |> LE.foldl1 (L.map2 <| L.map2 (*))
            |> fromJust
            |> L.concat

    in
    scores |> L.maximum |> fromJust

