module Day16 exposing(..)

import AocUtil exposing (..)
import Day16Input exposing (input)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set exposing (Set)
import String.Extra
import Tuple
import String as S
import List as L
import Debug as D exposing (log, todo)
import Array exposing (Array)
import Regex
import Dict exposing (Dict)

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

{-
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

        -}

parse: String -> Dict String Valve
parse input =
    input
    |> String.lines
    |> L.map parseLine
    |> L.map (\valve -> (valve.name, valve))
    |> Dict.fromList


type alias Path = { value: Int, steps: List { t: Int, to: String, who: Int }}

lookup: String -> Dict String a -> a
lookup n d =
    case Dict.get n d of
        Just v -> v
        Nothing -> Debug.todo (n ++ " not found in " ++ Debug.toString d)

type alias State = { people: List (String, Int) }

bestPath: Dict String Valve -> Dict String (Dict String Int) -> Int -> State -> Int -> String -> Path
bestPath valves cm time state bestSoFar indent =
    let
        -- _ = time |> log (indent++"time")
        -- _ = bestSoFar |> log (indent++"bestSoFar")
        -- _ = valves|>Dict.keys|>log (indent++"valves")
        -- _ = state|>log (indent++"state")
        potential =
            valves
            |> Dict.toList
            |> L.map (\(n,v) -> v.rate * time)
            |> L.foldl (+) 0
        -- _ = potential |> Debug.log (indent++"pot")
    in
    if potential < bestSoFar then
        let
            _=()
              |> log (indent++(D.toString time)++": "++(S.fromInt potential)++"<"++(S.fromInt bestSoFar)++","++"skipping")
        in
        { value=0, steps=[] }
    else
    state.people
    |> L.indexedMap (\who (from, walked)->
        let
            indent2 = indent ++ " [" ++(S.fromInt who) ++ "]"
            candidates =
                lookup from cm
                |> Dict.toList
                |> L.filter (\(n,c) -> Dict.member n valves)
                |> L.map (\(n,c)->(n,c-walked))
                |> L.filter (\(n,c)->c<time && c>=0)
                --|> log ("remaining from " ++ startValve.name ++ " at t=" ++ (D.toString time))

            -- _ = candidates|>log (indent2++"candidates("++(D.toString who)++")")

            best =
                candidates
                |> L.foldl (\(to,c) (i,bst) ->
                    let
                        addedValue = (time-c)*(lookup to valves).rate
                    in
                    bestPath
                        (Dict.remove to valves)
                        cm
                        (time - c)
                        (
                            { state | people = state.people
                                |> L.indexedMap (\who2 (from2, walked2)->
                                    if who2 == who then (to, 0)
                                    else (from2, walked2 + c))
                              }
                        )
                        (bst.value - addedValue)
                        (indent2++" "++S.fromInt i++" ")
                    |> (\p -> { p |
                            value=p.value + addedValue,
                            steps = {t=time-c, to=to,who=who}::p.steps })
                    |> (\p ->
                        (i+1,
                        if p.value > bst.value
                        then (p
                            --|>log (indent2++"new best")
                        )
                        else (bst
                            --|>log (indent2++"not better")
                        )))
                ) (0,{ value=0, steps=[] })
        in
        best |> Tuple.second
    )
    |> LE.maximumBy .value
    |> Maybe.withDefault { value=0, steps=[] }
    --|> log (indent++"result")

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

