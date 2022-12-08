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

        getAt (rowIndex,colIndex) = trees |> Array.get rowIndex |> fromJust |> Array.get colIndex |> fromJust

        walk : (Int, Int) -> (Int, Int) -> Int
        walk (dr, dc) (r0, c0) =
            let
                tree = getAt (r0, c0)

                loop n (r, c) =
                    let
                        z = if r < 0 || c < 0 || r >= rowCount || c >= colCount then
                                n
                            else if getAt (r,c) >= tree then
                                n + 1
                            else
                                loop (n + 1) (r+dr, c+dc)
                    in
                    z

            in
            loop 0 (r0+dr, c0+dc)


        scoresA =
            L.range 0 (rowCount - 1)
            |> L.concatMap (\rowIndex ->
                L.range 0 (colCount - 1)
                |> L.map (\colIndex -> (rowIndex, colIndex)))
            |> L.map (\(rowIndex, colIndex) ->
                  walk (0,1) (rowIndex, colIndex)
                * walk (1,0) (rowIndex, colIndex)
                * walk (-1,0) (rowIndex, colIndex)
                * walk (0,-1) (rowIndex, colIndex)
            )

    in
    scoresA |> L.maximum |> fromJust

