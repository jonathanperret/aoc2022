module Day18 exposing (..)

import Day18Input exposing (input)
import Debug as D
import List as L
import List.Extra as LE
import Set
import String as S

parseLine line =
    line
        |> S.split ","
        |> L.filterMap S.toInt


parse input =
    input
        |> String.lines
        |> L.filterMap
            (\line ->
                case parseLine line of
                    [ x, y, z ] ->
                        Just ( x, y, z )

                    _ ->
                        Nothing -- D.todo ("bad line " ++ line)
            )
        |> Set.fromList


neighbors ( x, y, z ) =
    [ ( x - 1, y, z )
    , ( x + 1, y, z )
    , ( x, y - 1, z )
    , ( x, y + 1, z )
    , ( x, y, z - 1 )
    , ( x, y, z + 1 )
    ]
        |> L.filter
            (\( x2, y2, z2 ) ->
                   x2 >= -1 && x2 <= 22
                && y2 >= -1 && y2 <= 22
                && z2 >= -1 && z2 <= 22
            )


findAir cubes =
    let
        fill visited frontier =
            let
                frontier2 =
                    frontier
                        |> L.concatMap neighbors
                        |> Set.fromList
                        |> Set.filter (\c -> not (Set.member c visited || Set.member c cubes))
            in
            if Set.isEmpty frontier2 then
                visited

            else
                fill (Set.union visited frontier2) (Set.toList frontier2)
    in
    fill Set.empty [(0,0,0)]


countSides cubes =
    cubes
        |> Set.toList
        |> L.map (neighbors >> LE.count (\n -> Set.member n cubes |> not))
        |> L.foldl (+) 0


part1 input =
    parse input
    |> countSides


part2 input =
    parse input
    |> findAir
    |> countSides

