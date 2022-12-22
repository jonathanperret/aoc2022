module Day22Tests exposing (..)

import AocUtil exposing (..)
import Day22Input exposing (input)
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


example1 = """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"""


suite : Test
suite =
    describe "day 22"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> Expect.equal 6032
            , test "input" <| \_ -> input |> part1 |> Expect.equal 0
            ]
        , skip <| describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 0
            , skip <| test "input" <| \_ -> input |> part2 |> Expect.equal 0
            ]
        ]

type alias Coords = (Int, Int)

type alias Tile =
    { coords: Coords
    , isWall: Bool
    , neighbors: Array Coords
    }


parseLine y line =
    line
    |> String.toList
    |> D.log "line"
    |> L.indexedMap (\i c ->
        if c == '#' then
            Just { coords = (i+1, y+1)
            , isWall = True
            , neighbors = Array.empty
            }
        else if c == '.' then
            Just { coords = (i+1, y+1)
            , isWall = False
            , neighbors = Array.empty
            }
        else Nothing)
    |> L.filterMap identity


parse input =
    let
        (mapStr, instructionsStr) =
            case input |> S.split "\n\n" of
                [ a, b ] -> (a, b)
                _ -> D.todo "bad input"

        rows = mapStr
            |> S.lines
            |> L.indexedMap parseLine

        tiles =
            rows
            |> L.concat

        width =
            tiles
            |> L.map (\t -> t.coords |> Tuple.first)
            |> L.maximum
            |> fromJust
            |> D.log "width"

        height =
            tiles
            |> L.map (\t -> t.coords |> Tuple.second)
            |> L.maximum
            |> fromJust
            |> D.log "height"

        dict =
            tiles
            |> L.map (\t -> (t.coords, t))
            |> Dict.fromList
            --|> D.log "dict"

        connected =
            tiles
            |> L.map (\t ->
                let
                    (x, y) = t.coords
                    right =
                        case Dict.get (x+1, y) dict of
                            Just t2 -> t2
                            Nothing ->
                                case L.range 1 x |> LE.findMap (\x2 -> Dict.get (x2, y) dict) of
                                    Just t2 -> t2
                                    Nothing -> D.todo "no right neighbor"
                    left =
                        case Dict.get (x-1, y) dict of
                            Just t2 -> t2
                            Nothing ->
                                case LE.reverseRange width x |> LE.findMap (\x2 -> Dict.get (x2, y) dict) of
                                    Just t2 -> t2
                                    Nothing -> D.todo "no left neighbor"

                    up =
                        case Dict.get (x, y-1) dict of
                            Just t2 -> t2
                            Nothing ->
                                case LE.reverseRange height y |> LE.findMap (\y2 -> Dict.get (x, y2) dict) of
                                    Just t2 -> t2
                                    Nothing -> D.todo "no up neighbor"
                    down =
                        case Dict.get (x, y+1) dict of
                            Just t2 -> t2
                            Nothing ->
                                case L.range 1 y |> LE.findMap (\y2 -> Dict.get (x, y2) dict) of
                                    Just t2 -> t2
                                    Nothing -> D.todo "no down neighbor"
                in
                { t | neighbors = Array.fromList [ right.coords, down.coords, left.coords, up.coords] }
                )

        map = connected |> L.map (\t -> (t.coords, t)) |> Dict.fromList

        instructions =
            Regex.find (Regex.fromString "(\\d+)([LR])" |> Maybe.withDefault Regex.never) instructionsStr
            |> L.map (\match ->
                case match.submatches of
                    [ Just lenStr, Just turn ] ->
                        (S.toInt lenStr |> fromJust, turn)
                    _ -> D.todo "bad instr"
                )

    in
    (map, instructions)


step map (len, turn) (coords, facing) =
    if len == 0 then
        let
            newFacing =
                case turn of
                    "L" -> modBy 4 (facing + 3)
                    "R" -> modBy 4 (facing + 1)
                    _ -> D.todo "bad turn"

            _ = newFacing |> D.log "turned to"
        in
        (coords, newFacing)
    else
    let
        currentTile = Dict.get coords map |> fromJust
        newCoords = Array.get facing currentTile.neighbors |> fromJust
        newTile = Dict.get newCoords map |> fromJust
        (newLen, newCoords2) =
            if newTile.isWall then
                (0, coords)
            else
                (len - 1, newCoords |> D.log "walked into")
    in
    step map (newLen, turn) (newCoords2, facing)


part1 input =
    let
        (map, instructions) =
            input
            |> parse
            |> D.log "parsed"

        startCoords =
            map
            |> Dict.keys
            |> LE.minimumBy (\(x,y) -> (y, x))
            |> fromJust
            |> D.log "start"

        initialState = (startCoords, 0)

        ((finalX, finalY), finalFacing) =
            instructions
            |> L.foldl (step map) initialState
            |> D.log "final"
    in
    1000 * finalY + 4 * finalX + finalFacing

part2: String -> Int
part2 input = 0
