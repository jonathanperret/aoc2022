module Day22 exposing (..)

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


type alias Coords = (Int, Int)

type alias Facing = Int

facingRight = 0
facingDown = 1
facingLeft = 2
facingUp = 3

facingStr facing =
    case facing of
        0 -> "right"
        1 -> "down"
        2 -> "left"
        3 -> "up"
        _ -> D.todo "bad facing"

type alias Tile =
    { coords: Coords
    , isWall: Bool
    , neighbors: Array (Coords, Facing)
    }

type alias Map = Dict Coords Tile

type alias State = (Coords, Facing)
type Instruction
    = TurnLeft
    | TurnRight
    | Advance

parseLine y line =
    line
    |> String.toList
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


parse : String -> (Map, List Instruction)
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

        dict =
            tiles
            |> L.map (\t -> (t.coords, t))
            |> Dict.fromList

        connected =
            tiles
            |> L.map (\t ->
                let
                    (x, y) = t.coords
                    right =
                        case Dict.get (x+1, y) dict of
                            Just t2 -> (t2.coords, facingRight)
                            Nothing ->
                                if y>=151 then
                                    ((y-100, 150), facingUp)
                                else if y>=101 then
                                    ((150, 151-y), facingLeft)
                                else if y >= 51 then
                                    ((y+50, 50), facingUp)
                                else
                                    ((100, 151-y), facingLeft)

                    left =
                        case Dict.get (x-1, y) dict of
                            Just t2 -> (t2.coords, facingLeft)
                            Nothing ->
                                if y>=151 then
                                    ((y-100, 1), facingDown)
                                else if y>=101 then
                                    ((51, 151-y), facingRight)
                                else if y >= 51 then
                                    ((y-50, 101), facingDown)
                                else
                                    ((1, 151-y), facingRight)


                    up =
                        case Dict.get (x, y-1) dict of
                            Just t2 -> (t2.coords, facingUp)
                            Nothing ->
                                if x>=101 then
                                    ((x-100, 200), facingUp)
                                else if x>=51 then
                                    ((1, x+100), facingRight)
                                else
                                    ((51, 50+x), facingRight)

                    down =
                        case Dict.get (x, y+1) dict of
                            Just t2 -> (t2.coords, facingDown)
                            Nothing ->
                                if x>=101 then
                                    ((100,x-50), facingLeft)
                                else if x>=51 then
                                    ((50, 100+x), facingLeft)
                                else
                                    ((x+100, 1), facingDown)
                in
                { t | neighbors = Array.fromList [ right, down, left, up] }
                )

        map = connected |> L.map (\t -> (t.coords, t)) |> Dict.fromList

        instructions =
            Regex.find (Regex.fromString "\\d+|L|R" |> Maybe.withDefault Regex.never) instructionsStr
            |> L.concatMap (\match ->
                case match.match of
                    "L" -> [ TurnLeft ]
                    "R" -> [ TurnRight ]
                    s ->
                        case S.toInt s of
                            Just len -> L.repeat len Advance
                            _ -> D.todo "bad len"
                    )

    in
    (map, instructions)

lookup: Map -> Coords -> Tile
lookup map (x, y) =
    case Dict.get (x, y) map of
        Just t -> t
        _ -> D.todo ("no tile at (" ++ S.fromInt x ++ "," ++ S.fromInt y ++ ")!")

step: Map -> Instruction -> State -> State
step map instr (coords, facing) =
    case instr of
        TurnLeft ->
            let
                newFacing = modBy 4 (facing + 3)
                --_ = newFacing |> facingStr |> D.log "// turned to"
            in (coords, newFacing)
        TurnRight ->
            let
                newFacing = modBy 4 (facing + 1)
                --_ = newFacing |> facingStr |> D.log "// turned to"
            in (coords, newFacing)
        Advance ->
            let
                currentTile = lookup map coords
                (newCoords, newFacing) = Array.get facing currentTile.neighbors |> fromJust
                newTile = lookup map newCoords
            in
                if newTile.isWall then
                    (coords, facing)
                else
                    ( newCoords
                        -- |> D.log ("// walked " ++ facingStr facing ++ " into")
                    , newFacing)

findStart: Map -> Coords
findStart map =
    map
    |> Dict.keys
    |> LE.minimumBy (\(x,y) -> (y, x))
    |> fromJust
    |> D.log "// start"


makePath: Map -> List Instruction -> List State
makePath map instructions =
    let
        initialState = (findStart map, 0)

        steps =
            instructions
            |> LE.scanl (step map)
                initialState
    in
    steps
    --|> D.log "// path"


part2 inp =
    let
        (map, instructions) =
            inp
            |> parse
            --|> D.log "parsed"

        initialState = (findStart map, 0)

        ((finalX, finalY), finalFacing) =
            instructions
            |> L.foldl (step map) initialState
            |> D.log "final"
    in
    1000 * finalY + 4 * finalX + finalFacing |> D.log "result"
