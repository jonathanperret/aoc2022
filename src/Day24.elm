module Day24 exposing (..)

import AocUtil exposing (..)
import Day23 exposing (Pos, dirVec)
import Debug as D
import List as L
import List.Extra as LE
import Set exposing (Set)
import String as S
import String.Interpolate exposing (interpolate)
import Test exposing (..)


type alias Pos =
    ( Int, Int )


type alias Dir =
    Char


north : Dir
north =
    '^'


east : Dir
east =
    '>'


south : Dir
south =
    'v'


west : Dir
west =
    '<'


dirs : List Dir
dirs =
    [ north
    , east
    , south
    , west
    ]


type alias Blizzard =
    ( Pos, Dir )


type alias Map =
    { width : Int
    , height : Int
    , blizzards : Set Blizzard
    }


dirVec : Dir -> Pos
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
            D.todo ("bad dir " ++ D.toString dir)


posAdd : Pos -> Pos -> Pos
posAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


posMul : Int -> Pos -> Pos
posMul n ( x, y ) =
    ( n * x, n * y )


posWrap : Map -> Pos -> Pos
posWrap map ( x, y ) =
    ( modBy map.width x, modBy map.height y )


neighbors : Pos -> List Pos
neighbors ( x, y ) =
    [ ( x, y - 1 ) -- N
    , ( x + 1, y ) -- E
    , ( x, y + 1 ) -- S
    , ( x - 1, y ) -- W
    ]


potentialBlizzardAt : Map -> Pos -> Dir -> Blizzard
potentialBlizzardAt map pos dir =
    ( pos, dir )


posFreeAt : Map -> Pos -> Bool
posFreeAt map pos =
    isStart map pos
        || isGoal map pos
        || (isInMap map pos
                && not
                    ([ north, south, east, west ]
                        |> L.any
                            (\dir ->
                                map.blizzards
                                    |> Set.member (potentialBlizzardAt map pos dir)
                            )
                    )
           )


parseLine : Int -> String -> Maybe (List Blizzard)
parseLine row line =
    let
        chars =
            S.toList line
    in
    case chars of
        _ :: _ :: '#' :: _ ->
            Nothing

        _ ->
            chars
                |> L.indexedMap
                    (\col char ->
                        if L.member char dirs then
                            Just ( ( col - 1, row - 1 ), char )

                        else
                            Nothing
                    )
                |> L.filterMap identity
                |> Just


parse : String -> Map
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
    , blizzards = blizzards
    }


renderMap : Map -> Set Pos -> String
renderMap map poss =
    L.range -1 map.height
        |> L.map
            (\y ->
                L.range -1 map.width
                    |> L.map
                        (\x ->
                            if x < 0 || x >= map.width || (y < 0 && x /= 0) || (y >= map.height && x /= (map.width - 1)) then
                                '#'

                            else if Set.member ( x, y ) poss then
                                if posFreeAt map ( x, y ) then
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
                                        |> L.map (\dir -> potentialBlizzardAt map ( x, y ) dir)
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


startPos : Map -> Pos
startPos _ =
    ( 0, -1 )


goalPos : Map -> Pos
goalPos map =
    ( map.width - 1, map.height )


isStart : Map -> Pos -> Bool
isStart map pos =
    pos == startPos map


isGoal : Map -> Pos -> Bool
isGoal map pos =
    pos == goalPos map


isInMap : Map -> Pos -> Bool
isInMap map ( x, y ) =
    x >= 0 && x < map.width && y >= 0 && y < map.height


nextStates : Map -> Pos -> List Pos
nextStates map pos =
    (pos :: neighbors pos)
        |> L.filter (posFreeAt map)
        |> L.map (\newPos -> newPos)


updateMap : Map -> Map
updateMap map =
    { map
        | blizzards =
            map.blizzards
                |> Set.map (\( pos, dir ) -> ( posWrap map (posAdd pos (dirVec dir)), dir ))
    }


solve : Map -> Int -> Pos -> Pos -> Maybe ( Int, Map )
solve map0 time0 start goal =
    let
        step map time frontier =
            let
                map2 =
                    updateMap map

                --_ = frontier |> D.log "frontier"
                frontier2 =
                    frontier
                        |> Set.toList
                        |> List.concatMap (nextStates map2)
                        |> Set.fromList

                --|> D.log "frontier2"
                _ =
                    \_ ->
                        ("after minute " ++ S.fromInt time) |> D.log (renderMap map2 frontier2)

                goalCost =
                    if Set.member goal frontier2 then
                        Just time

                    else
                        Nothing
            in
            case goalCost of
                Just c ->
                    Just ( c, map2 )

                Nothing ->
                    if Set.isEmpty frontier2 then
                        Nothing

                    else
                        step map2 (time + 1) frontier2
    in
    step map0 (time0 + 1) (Set.singleton start)


solveTrip : Map -> Int -> List Pos -> Maybe Int
solveTrip map time waypoints =
    case waypoints of
        from :: to :: rest ->
            solve map time from to
                |> Maybe.map
                    (\( c, m ) ->
                        D.log (interpolate "{0} -> {1} at {2}" [ D.toString from, D.toString to, D.toString time ]) c
                            |> (\_ -> ( c, m ))
                    )
                |> Maybe.andThen (\( arrivalTime, map2 ) -> solveTrip map2 arrivalTime (to :: rest))

        _ ->
            Just time


part1 : String -> Maybe Int
part1 input =
    let
        map =
            input
                |> parse
    in
    solveTrip map 0 [ startPos map, goalPos map ]


part2 : String -> Maybe Int
part2 input =
    let
        map =
            input
                |> parse
    in
    solveTrip map 0 [ startPos map, goalPos map, startPos map, goalPos map ]
