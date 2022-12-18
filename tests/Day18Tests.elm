module Day18Tests exposing (..)

import AocUtil exposing (..)
import Day18Input exposing (input)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set exposing (Set)
import String.Extra
import Test exposing (..)
import Tuple
import String as S
import List as L
import Debug as D
import Array exposing (Array)
import Regex
import Dict exposing (Dict)


example1 = """2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"""


suite : Test
suite =
    describe "day 18"
        [ describe "part 1"
            [ test "sides" <| \_ -> (0,0,0) |> sides |> Expect.equal
                [(-0.5,0,0),(0.5,0,0),(0,-0.5,0),(0,0.5,0),(0,0,-0.5),(0,0,0.5)]
            , test "example0" <| \_ -> "1,1,1\n2,1,1" |> part1 |> Expect.equal 10
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 64
            , test "input" <| \_ -> input |> part1 |> Expect.equal 4548
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 58
            , test "input" <| \_ -> input |> part2 |> Expect.equal 2588
            ]
        ]

parseLine line =
    line
    |> S.split ","
    |> L.filterMap S.toFloat

parse input =
    input
    |> String.lines
    |> L.map parseLine
    |> L.filterMap (\a -> case a of
        [x,y,z] -> Just (x,y,z)
        _ -> Nothing)

neighbors (x,y,z) =
    [
         (x-1,y,z)
        ,(x+1,y,z)
        ,(x,y-1,z)
        ,(x,y+1,z)
        ,(x,y,z-1)
        ,(x,y,z+1)
    ]
    |> L.filter (\(x2,y2,z2) ->
        x2>= -1 && x2<=22
        && y2>= -1 && y2<=22
        && z2>= -1 && z2<=22)


fill blocks visited frontier =
    let
        frontier2 =
            frontier
            |> L.concatMap neighbors
            |> L.filter (\c -> not (Set.member c visited || Set.member c blocks))
            |> Set.fromList
    in
    if Set.isEmpty frontier2 then
        visited
    else
        fill blocks (Set.union visited frontier2) (Set.toList frontier2)


sides (x,y,z) =
    [
         (x-0.5,y,z)
        ,(x+0.5,y,z)
        ,(x,y-0.5,z)
        ,(x,y+0.5,z)
        ,(x,y,z-0.5)
        ,(x,y,z+0.5)
    ]
    |> L.filter (\(x2,y2,z2) ->
        x2>= -1 && x2<=22
        && y2>= -1 && y2<=22
        && z2>= -1 && z2<=22
    )

keepUnique ss =
    ss
    |> L.foldl (\side dict ->
        Dict.update
            side
            (\found -> case found of
                Just _ -> Just False
                Nothing -> Just True
            )
            dict
    ) Dict.empty
    |> Dict.filter (\_ b -> b)
    |> Dict.keys
    --|> D.log "uniqueSides"

part1 input =
    let
        cubes =
            input
            |> parse

        allSides =
            cubes
            |> L.concatMap sides

        uniqueSides =
            allSides
            |> keepUnique
    in
    uniqueSides |> L.length

part2 input =
    let
        cubes =
            input
            |> parse

        air =
            fill
                (cubes |> Set.fromList)
                (Set.singleton (0,0,0))
                [(0,0,0)]
                --|> D.log "air"

        _ = \_ -> air
            |> Set.toList
            |> L.map (\(x,y,z) -> "translate(["
                ++ S.fromFloat x ++ ","
                ++ S.fromFloat y ++ ","
                ++ (if z >= 10 then "off+" else "")
                ++ S.fromFloat z ++ "]) cube(airsize);")
            |> S.join "\n"
            |> (\s -> D.log s ())

        airSides =
            air
            |> Set.toList
            |> L.concatMap sides

        uniqueSides =
            airSides
            |> keepUnique
    in
    uniqueSides |> L.length

