module Day16Tests exposing (..)

import AocUtil exposing (..)
import Day16Input exposing (input)
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
import Debug as D exposing (log, todo)
import Array exposing (Array)
import Regex
import Dict exposing (Dict)


example1 = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"""

suite : Test
suite =
    describe "day 16"
        [ describe "part 1"
            [ test "parseLine" <|\_-> "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
                |> parseLine |> Expect.equal { name="AA", rate=0, next=["DD","II","BB"] }
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 1651
            , test "input" <| \_ -> input |> part1 |> Expect.equal 0
            ]
        , skip <| describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 0
            , skip <| test "input" <| \_ -> input |> part2 |> Expect.equal 0
            ]
        ]

type alias Valve = { name: String, rate: Int, next: List String }

parseLine: String -> Valve
parseLine line =
    let
        re = Regex.fromString "[A-Z][A-Z]|\\d+"|>Maybe.withDefault Regex.never
    in
    case Regex.find re line |> List.map .match of
        name::rateStr::next ->
            { name=name, rate=rateStr|>String.toInt|>fromJust, next=next }
        _ -> Debug.todo ("bad line " ++ line)

dot =
    let
        valves=
            example1
            |> String.lines
            |> L.map parseLine
    in
    Debug.log (
        "graph G {\n"
        ++
        (
            valves
            |> L.map (\v -> "  "++v.name++"[label=\""++v.name++"("++(v.rate|>String.fromInt)++")\"]\n")
            |> String.concat
        )
        ++
        (
            valves
            |> L.concatMap (\v -> v.next |> L.map (\n -> if n < v.name then "  "++v.name++" -- "++n++"\n" else ""))
            |> String.concat
        ) ++ "}\n"
    ) ()


parse: String -> Dict String Valve
parse input =
    input
    |> String.lines
    |> L.map parseLine
    |> L.map (\valve -> (valve.name, valve))
    |> Dict.fromList


type alias Path = List (Int, Int, String)

lookup: String -> Dict String a -> a
lookup n d =
    case Dict.get n d of
        Just v -> v
        Nothing -> Debug.todo (n ++ " not found in " ++ Debug.toString d)

enumPaths: Dict String Valve -> Dict String (Dict String Int) -> Int -> Valve -> List Path
enumPaths valves cm time startValve =
    let
        _ = time -- |> Debug.log (startValve.name ++ " at " ++ Debug.toString valves)
        valves2 = Dict.remove startValve.name valves

        myCosts = Dict.get startValve.name cm |> fromJust

        candidates =
            myCosts
            |> Dict.toList
            |> L.filter (\(n,c) -> Dict.member n valves2)
            --|> log ("moves from " ++ startValve.name ++ " at t=" ++ (D.toString time))

        remaining = candidates |> L.filter (\(n,c)->c<time)
            --|> log ("remaining from " ++ startValve.name ++ " at t=" ++ (D.toString time))

        pathsIfMoving =
            remaining
            |> L.concatMap (\(n,c) ->
                enumPaths
                    valves2
                    cm
                    (time - c)
                    (lookup n valves)
                |> L.map (\p -> (time-c, (lookup n valves).rate, n)::p)
            )

        next = [] :: pathsIfMoving
            --|> Set.fromList |> Set.toList
            -- |> L.filter (L.isEmpty >> not)
            --|> Debug.log ("next ")
    in
    next
    --|> log ("\nenumPaths "++(time|>S.fromInt)++" "++ startValve.name++" costs="++(myCosts|>Dict.toList|>D.toString)++" "++(valves|>Dict.values|>D.toString))

evalPath: Path -> Int
evalPath p =
    p
    |> L.map (\(t,v,n) ->
        t * v
    )
    |> L.foldl (+) 0

-- time required to open a given valve when standing on another
costMatrix: Dict String Valve -> Dict String (Dict String Int)
costMatrix valves =
    let
        pathLengths : Valve -> Dict String Int
        pathLengths valve =
            let
                step : { edge: List (Int, String), vis: Set String, res: Dict String Int } -> Dict String Int
                step state =
                    let
                        (len, name) = L.head state.edge |> fromJust
                        rest = L.drop 1 state.edge

                        v = lookup name valves
                        next =
                            v.next
                            |> L.filter (\n -> Set.member n state.vis |> not)

                        vis2 = Set.union state.vis (next |> Set.fromList)

                        res2 =
                            next
                            |> L.filter (\n -> (lookup n valves).rate > 0)
                            |> L.map (\n -> (len+1,n))
                            |> L.foldl (\(c, n) d -> Dict.insert n c d) state.res

                        state2 = { state | vis = vis2, res = res2,
                            edge=rest ++ (next|>L.map(\n->(len+1,n))) }
                    in
                    if List.isEmpty state2.edge then
                        state2.res
                    else
                        step state2

            in
            step { edge=[(0, valve.name)], vis=Set.singleton valve.name, res=Dict.empty }

    in
    valves
    |> Dict.filter (\n v -> n=="AA" || v.rate>0)
    |> Dict.map (\_ v -> pathLengths v)
    -- Add cost of opening
    |> Dict.map (\_ d -> Dict.map (\_ c -> c+1) d)
    --|> Debug.log "costMatrix"

part1 input =
    let
        dict =
            input
            |> parse
            --|> Debug.log "initial"

        cm = costMatrix dict

        _ = dict
            --|> Debug.log ("dict " ++ (Debug.toString (Dict.size dict)))

        ps = enumPaths dict cm 30 (lookup "AA" dict)
            |> List.sort
            --|> List.length
            --|> Debug.log "ps"

        _ = ps |> L.length |> Debug.log "found"

        best = ps
            |> L.map (\p -> (evalPath p, L.map (\(t,v,n)->(Debug.toString t++":"++Debug.toString v++":"++n)) p |> String.join " -> "))
            |> L.sort
            |> L.reverse
            |> L.head |> fromJust
            |> log "best"

        --ps2 = ps |> Set.fromList |> Set.toList |> L.length |> Debug.log "ps2"
    in
    best |> Tuple.first

{-
20*(30-2)+13*(30-5)+11*(30-9)+22*(30-17)+3*(30-21)+2*(30-24)
-}

part2: String -> Int
part2 input = 0
