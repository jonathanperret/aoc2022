module Day24 exposing (..)

import AocUtil exposing (..)
import Arithmetic
import Array exposing (Array, initialize)
import Day23 exposing (Pos)
import Debug as D
import Dict exposing (Dict, intersect)
import Expect
import Fuzz
import Html exposing (blockquote)
import List as L
import List.Extra as LE exposing (foldl1)
import Regex
import Set exposing (Set)
import String as S
import String.Extra
import String.Interpolate exposing (interpolate)
import Test exposing (..)
import Tuple exposing (first)


type alias Pos =
    ( Int, Int )


type alias Dir =
    Char


north =
    '^'


east =
    '>'


south =
    'v'


west =
    '<'


dirs =
    [ north
    , east
    , south
    , west
    ]


type alias Blizzard =
    ( Pos, Dir )


dirVec dir =
    case dir of
        '^' ->
            -- N
            ( 0, -1 )

        '>' ->
            -- E
            ( 1, 0 )

        'v' ->
            -- S
            ( 0, 1 )

        '<' ->
            -- W
            ( -1, 0 )

        _ ->
            D.todo "bad dir"


posAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


posMul n ( x, y ) =
    ( n * x, n * y )


posMod map ( x, y ) =
    ( modBy map.width x, modBy map.height y )


neighbors ( x, y ) =
    [ ( x, y - 1 ) -- N
    , ( x + 1, y ) -- E
    , ( x, y + 1 ) -- S
    , ( x - 1, y ) -- W
    ]


blizzardAt map time ( pos, dir ) =
    posMod map (posAdd pos (posMul time (dirVec dir)))


blizzardOccupies map time pos blizzard =
    blizzardAt map time blizzard == pos


type alias Map =
    { width : Int
    , height : Int
    , cycleTime : Int
    , blizzards : Set Blizzard
    }


potentialBlizzardAt : Map -> Int -> Pos -> Dir -> Blizzard
potentialBlizzardAt map time pos dir =
    ( posMod map (posAdd pos (posMul -time (dirVec dir))), dir )


posFreeAt map time pos =
    isStart map pos
        || isGoal map pos
        || (isInMap map pos
                && not
                    ([ north, south, east, west ]
                        |> L.any
                            (\dir ->
                                map.blizzards
                                    |> Set.member (potentialBlizzardAt map time pos dir)
                            )
                    )
           )


parseLine row line =
    let
        chars =
            S.toList line
    in
    case LE.getAt 2 chars of
        Just '#' ->
            Nothing

        _ ->
            chars
                |> L.indexedMap
                    (\col char ->
                        case char of
                            '>' ->
                                Just ( ( col - 1, row - 1 ), east )

                            '<' ->
                                Just ( ( col - 1, row - 1 ), west )

                            '^' ->
                                Just ( ( col - 1, row - 1 ), north )

                            'v' ->
                                Just ( ( col - 1, row - 1 ), south )

                            _ ->
                                Nothing
                    )
                |> L.filterMap identity
                |> Just


parse input =
    let
        lines =
            S.lines input

        mapRows =
            lines
                |> L.indexedMap parseLine
                |> L.filterMap identity

        width =
            case LE.getAt 1 lines of
                Just s ->
                    S.length s - 2

                Nothing ->
                    0

        height =
            L.length mapRows

        blizzards =
            mapRows
                |> L.concat
                |> Set.fromList
    in
    { width = width
    , height = height
    , cycleTime = Arithmetic.lcm width height
    , blizzards = blizzards
    }


renderMap : Map -> Int -> List Pos -> String
renderMap map time poss =
    L.range -1 map.height
        |> L.map
            (\y ->
                L.range -1 map.width
                    |> L.map
                        (\x ->
                            if x < 0 || x >= map.width || (y < 0 && x /= 0) || (y >= map.height && x /= (map.width - 1)) then
                                '#'

                            else if L.member ( x, y ) poss then
                                if posFreeAt map time ( x, y ) then
                                    'E'

                                else
                                    'X'

                            else if isStart map ( x, y ) then
                                'S'

                            else if isGoal map ( x, y ) then
                                'G'

                            else
                                case
                                    dirs
                                        |> L.map (\dir -> potentialBlizzardAt map time ( x, y ) dir)
                                        |> L.filter (\b -> Set.member b map.blizzards)
                                of
                                    [] ->
                                        '.'

                                    [ ( _, d ) ] ->
                                        d

                                    l ->
                                        Char.fromCode (L.length l + 48)
                        )
                    |> S.fromList
            )
        |> S.join "\n"
        |> (\s -> S.append s "\n")


type alias State =
    ( Pos, Int )


startPos _ =
    ( 0, -1 )


goalPos map =
    ( map.width - 1, map.height )


isStart map pos =
    pos == startPos map


isGoal map pos =
    pos == goalPos map


isInMap map ( x, y ) =
    isStart map ( x, y )
        || isGoal map ( x, y )
        || (x >= 0 && x < map.width && y >= 0 && y < map.height)


nextStates : Map -> State -> List State
nextStates map ( pos, time ) =
    (pos :: neighbors pos)
        |> L.filter (posFreeAt map (time + 1))
        |> L.map (\newPos -> ( newPos, modBy map.cycleTime (time + 1) ))


solve : Map -> State -> Pos -> Maybe Int
solve map start goal =
    let
        visited0 =
            [ ( start, 0 ) ] |> Dict.fromList

        frontier0 =
            [ start ]

        step time { visited, frontier } =
            let
                --_ = frontier |> D.log "frontier"
                frontier2 =
                    frontier
                        |> List.concatMap (nextStates map)
                        |> Set.fromList
                        |> Set.filter (\state -> Dict.member state visited |> not)
                        |> Set.toList

                --|> D.log "frontier2"
                _ =
                    \_ ->
                        ("after minute " ++ S.fromInt time) |> D.log (renderMap map time (frontier2 |> L.map Tuple.first))

                visited2 =
                    frontier2
                        |> List.foldl (\st v -> Dict.insert st time v) visited

                goalCost =
                    frontier2
                        |> LE.findMap
                            (\( pos, _ ) ->
                                if pos == goal then
                                    Just time

                                else
                                    Nothing
                            )
            in
            case goalCost of
                Just _ ->
                    goalCost

                Nothing ->
                    if List.isEmpty frontier2 then
                        Nothing

                    else
                        step (time + 1) { visited = visited2, frontier = frontier2 }
    in
    step 1 { visited = visited0, frontier = frontier0 }


solveTrip map time waypoints =
    case waypoints of
        from :: to :: rest ->
            solve map ( from, time ) to
                |> D.log (interpolate "{0} -> {1}" [ D.toString from, D.toString to ])
                |> Maybe.andThen
                    (\leg ->
                        solveTrip map (time + leg) (to :: rest)
                            |> Maybe.map ((+) leg)
                    )

        _ ->
            Just 0


part1 input =
    let
        map =
            input
                |> parse
    in
    solveTrip map 0 [ startPos map, goalPos map ]


part2 input =
    let
        map =
            input
                |> parse
    in
    solveTrip map 0 [ startPos map, goalPos map, startPos map, goalPos map ]
