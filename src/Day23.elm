module Day23 exposing (..)

import Day23Input exposing (..)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set exposing (Set)
import Dict exposing (Dict)
import String.Extra
import Tuple
import String as S
import List as L
import Debug as D
import Array exposing (Array)
import Regex
import String.Interpolate exposing (interpolate)


type Dir
    = North
    | South
    | East
    | West

type alias Pos = (Int, Int)

type alias Map = Set Pos

dirVec dir = case dir of
    North -> (0, -1)
    South -> (0, 1)
    East -> (1, 0)
    West -> (-1, 0)

posAdd (x1, y1) (x2, y2) =
    (x1 + x2, y1 + y2)

neighbors (x, y) =
    [ (x, y-1)   -- N
    , (x+1, y-1) -- NE
    , (x+1, y)   -- E
    , (x+1, y+1) -- SE
    , (x, y+1)   -- S
    , (x-1, y+1) -- SW
    , (x-1, y)   -- W
    , (x-1, y-1) -- NW
    ]

existingNeighbors map (x, y) =
    neighbors (x, y)
    |> L.map (\p -> Set.member p map)


parse input =
    input
    |> S.lines
    |> L.indexedMap (\y line ->
        line
        |> S.toList
        |> L.indexedMap (\x c -> case c of
            '#' -> Just (x, y)
            _ -> Nothing
        ))
    |> L.concat
    |> L.filterMap identity
    |> Set.fromList

rotateLeft n list =
    let
        n2 = modBy (L.length list) n
        (left, right) = LE.splitAt n2 list
    in
    right ++ left

proposal: Map -> Int -> Pos -> Pos
proposal map round pos =
    let
        nbs = existingNeighbors map pos
        validDirections =
            case nbs of
                [False, False, False, False, False, False, False, False] -> [Nothing, Nothing, Nothing, Nothing]
                [n, ne, e, se, s, sw, w, nw] ->
                    [ if not (n || ne || nw) then Just North else Nothing
                    , if not (s || se || sw) then Just South else Nothing
                    , if not (w || nw || sw) then Just West else Nothing
                    , if not (e || ne || se) then Just East else Nothing
                    ]
                _ -> [] -- D.todo "bad neighbors"
        rotatedDirections = rotateLeft round validDirections
        selectedDirection = rotatedDirections |> L.filterMap identity |> L.head
        proposedLocation = case selectedDirection of
            Just dir -> posAdd pos (dirVec dir)
            _ -> pos
        _ = (pos, (validDirections, rotatedDirections, selectedDirection), proposedLocation)
        --|> D.log "pos,valid,rotated,selected,proposedLocation"
    in
    proposedLocation


proposals: Map -> Int -> List (Pos, Pos)
proposals map round =
    map
    |> Set.toList
    |> L.map (\pos -> (pos, proposal map round pos))

applyRound: Map -> Int -> (Map, Int)
applyRound map round =
    let
        props = proposals map round
            --|>D.log "proposals"

        (tofrom, _, moveCount) =
            props
            |> L.foldl (\(from, to) (dict, conflicts, cnt) ->
                case Set.member to conflicts of
                    True -> (Dict.insert from from dict, conflicts, cnt)
                    False ->
                        case Dict.get to dict of
                            Nothing ->
                                ( Dict.insert to from dict
                                , conflicts
                                , if to /= from then cnt+1 else cnt
                                )
                            Just from2 ->
                                ( dict
                                  |> Dict.remove to
                                  |> Dict.insert from2 from2
                                  |> Dict.insert from from
                                , conflicts
                                  |> Set.insert to
                                , cnt-1
                                )
             ) (Dict.empty, Set.empty, 0)

        newMap =
            tofrom
            |> Dict.keys
            |> Set.fromList

--        conflicted =
--            props
--            |> L.map Tuple.second
--            |> LE.frequencies
--            |> L.filterMap (\(to, n) -> if n > 1 then Just to else Nothing)
--            |> Set.fromList
--
--        uniqueProposals =
--            props
--            |> L.map (\(from, to) ->
--                if Set.member to conflicted then from else to)
--
--        newMap = uniqueProposals
--            |> Set.fromList


--        _ = if Set.size map /= Set.size newMap then
--                D.todo (interpolate "oops, map size changed from {0} to {1}" [Set.size map|>S.fromInt, Set.size newMap|>S.fromInt])
--            else
--                ()
    in
    (newMap, moveCount)

renderMap: Map -> String
renderMap map =
    if Set.isEmpty map
    then "empty"
    else
    let
        list = map |> Set.toList
    in
    case
        [ list |> L.map Tuple.first |> L.minimum
        , list |> L.map Tuple.first |> L.maximum
        , list |> L.map Tuple.second |> L.minimum
        , list |> L.map Tuple.second |> L.maximum
        ] |> L.filterMap identity of
        [ minX, maxX, minY, maxY ] ->
            L.range (if minY>0 then 0 else minY) (maxY + 1)
            |> L.map (\y ->
                L.range (if minX>0 then 0 else minX) (maxX + 1)
                |> L.map (\x ->
                    if Set.member (x, y) map then '#' else '.'
                )
                |> S.fromList
            )
            |> S.join "\n"
        _ -> "bad map"


allRounds: Map -> Int -> Int -> (Int, Map)
allRounds map limit round =
    if round >= limit then (round, map)
    else
    let
        (newMap, moveCount) = applyRound map round

        -- _ = round |> D.log (renderMap newMap)
    in
    if moveCount > 0 then
        allRounds newMap limit (round + 1)
    else
        (round, map)


countGround: Map -> Int
countGround map =
    let
        list = map |> Set.toList
        groundTiles = case
            [ list |> L.map Tuple.first |> L.minimum
            , list |> L.map Tuple.first |> L.maximum
            , list |> L.map Tuple.second |> L.minimum
            , list |> L.map Tuple.second |> L.maximum
            ] |> L.filterMap identity of
            [ minX, maxX, minY, maxY ] ->
                L.range minY maxY
                    |> L.concatMap (\y ->
                        L.range minX maxX
                        |> L.map (\x ->
                            if Set.member (x, y) map then Nothing else Just (x,y)
                        )
                    )
                    |> L.filterMap identity
            _ -> []
    in
    groundTiles |> L.length

part1 input =
    let
        map =
            input
            |> parse
            --|> D.log "parsed"

        -- _ = "initial" |> D.log (renderMap map)

        (_, finalMap) = allRounds map 10 0

        _ = renderMap finalMap

    in
    countGround finalMap

part2: String -> Int
part2 input =
    let
        map =
            input
            |> parse
            --|> D.log "parsed"

        (finalRound, _) = allRounds map 1000 0

    in
    finalRound + 1

