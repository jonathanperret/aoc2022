module Day12Tests exposing (..)

import AocUtil exposing (..)
import Day12Input exposing (input)
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
import Dict


example1 = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""


suite : Test
suite =
    describe "day 12"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> Expect.equal 31
            , test "input" <| \_ -> input |> part1 |> Expect.equal 425
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 29
            , test "input" <| \_ -> input |> part2 |> Expect.equal 418
            , test "example brute" <| \_ -> example1 |> part2brute |> Expect.equal 29
            , test "input brute" <| \_ -> input |> part2brute |> Expect.equal 418
            ]
        ]

parse input =
    let
        grid = input
            |> String.lines
            |> List.map (String.toList)
        startRow = grid |> LE.findIndex (L.any ((==) 'S')) |> fromJust
        startCol = grid |> LE.getAt startRow |> fromJust |> LE.findIndex ((==) 'S') |> fromJust
        endRow = grid |> LE.findIndex (L.any ((==) 'E')) |> fromJust
        endCol = grid |> LE.getAt endRow |> fromJust |> LE.findIndex ((==) 'E') |> fromJust
    in
    { grid = grid
        |> List.map (List.map (\c ->
            case c of
                'S' -> 0
                'E' -> 25
                _ -> (Char.toCode c) - (Char.toCode 'a')
            ))
    , start=(startRow, startCol)
    , end=(endRow, endCol)
    }

cellAt: (Int, Int) -> List (List Int) -> Maybe Int
cellAt (r, c) grid =
    grid
    |> LE.getAt r
    |> Maybe.andThen (LE.getAt c)

neighbors: (Int, Int) -> List (List Int) -> List ((Int, Int), Int)
neighbors (r,c) grid =
    [(-1,0),(1,0),(0,-1),(0,1)]
    |> List.map (\(dr,dc) -> (r+dr,c+dc))
    |> List.filterMap (\(r2,c2) ->
        cellAt (r2, c2) grid
        |> Maybe.map (\x -> ((r2,c2), x)))

solve { grid, start, end } =
    let
        visited0 = [ (start, 0) ] |> Dict.fromList
        edge0 = [ (start, 0) ]

        step { visited, edge } =
            let
                edge2 =
                    edge
                    |> List.concatMap (\ ((r,c),cost) ->
                            let
                                h = cellAt (r,c) grid |> fromJust
                                nb = neighbors (r,c) grid
                                    |> List.filter (\(_,h2) -> h2 <= h+1)
                                    |> List.map (\(pos,_) -> pos)
                                    |> List.filter (\pos -> Dict.member pos visited |> not)
                            in
                            nb |> List.map (\pos -> (pos, cost+1))
                        )
                    |> List.sortBy (\(pos,cost)->cost)
                    |> LE.uniqueBy (\(pos,cost)->pos)

                visited2 = List.foldl (\(pos, cost) d -> Dict.insert pos cost d) visited edge2
            in
            if List.isEmpty edge2 || Dict.member end visited2 then
                Dict.get end visited2
            else
                step { visited=visited2, edge=edge2 }

    in
    step { visited=visited0, edge=edge0 }

reverseSolve { grid, end } =
    let
        visited0 = [ (end, 0) ] |> Dict.fromList
        edge0 = [ (end, 0) ]

        step { visited, edge } =
            let
                next =
                    edge
                    |> List.concatMap (\ ((r,c),cost) ->
                            let
                                h = cellAt (r,c) grid |> fromJust
                            in
                                neighbors (r,c) grid
                                    |> List.filter (\(_,h2) -> h <= h2+1)
                                    |> List.filter (\(pos,_) -> not <| Dict.member pos visited)
                                    |> List.map (\(pos,h2) -> ((pos, cost+1),h2))
                        )

                edge2 = next
                    |> List.map Tuple.first
                    |> List.sortBy (\(pos,cost)->cost)
                    |> LE.uniqueBy (\(pos,cost)->pos)

                found = next
                    |> List.filterMap (\((_,cost),h) -> if h==0 then Just cost else Nothing)

                visited2 = List.foldl (\(pos, cost) -> Dict.insert pos cost) visited edge2
            in
            case List.minimum found of
                Just x -> x
                Nothing -> step { visited=visited2, edge=edge2 }

    in
    step { visited=visited0, edge=edge0 }

part1 input =
    input |> parse |> solve |> fromJust

part2: String -> Int
part2 input =
    input |> parse |> reverseSolve

part2brute: String -> Int
part2brute input =
    let
        ({grid, start, end} as problem) = input |> parse

        candidates : List (Int, Int)
        candidates =
            grid
            |> List.indexedMap (\r row ->
                row
                |> List.indexedMap (\c cell ->
                    if cell == 0 then
                        Just (r,c)
                    else Nothing))
            |> List.concat
            |> List.filterMap identity
            |> Debug.log "candidates"

        costs : List Int
        costs =
            candidates
            |> List.filterMap (\pos -> solve { grid=grid, start=pos, end=end })
            |> Debug.log "costs"
    in
    List.minimum costs |> fromJust

