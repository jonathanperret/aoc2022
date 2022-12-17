module Day17Tests exposing (..)

import AocUtil exposing (..)
import Day17Input exposing (input)
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
import Integer

rockShapes = [
      [(0,2),(0,3),(0,4),(0,5)]
    , [(0,3),(1,2),(1,3),(1,4),(2,3)]
    , [(0,2),(0,3),(0,4),(1,4),(2,4)]
    , [(0,2),(1,2),(2,2),(3,2)]
    , [(0,2),(0,3),(1,2),(1,3)]
    ]

example1 = """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"""

suite : Test
suite =
    describe "day 17"
        [ describe "part 2"
            [
              test "dropOne" <| \_ -> dropOne emptyField ">>><oooooo"
                [(3,2),(3,3),(3,4),(3,5)]
                |> Expect.equal
                (Set.fromList [(0,2),(0,3),(0,4),(0,5)], "oooooo", 3)
            , test "dropOne2" <| \_ -> dropOne
                (Set.fromList [(0,2),(0,3),(0,4),(0,5)])
                "<><>oooo"
                ([(4,3),(5,2),(5,3),(5,4),(6,3)])
                |> Expect.equal
                (Set.fromList [(0,2),(0,3),(0,4),(0,5),
                    (1,3),(2,2),(2,3),(2,4),(3,3)], "oooo", 3)
            , test "dropNew" <| \_ -> dropNew emptyField ">>><ooo"
                0
                |> Expect.equal (Set.fromList [(0,2),(0,3),(0,4),(0,5)], "ooo", 3)
            , test "example short" <| \_ -> example1 |> part2 210 "2022" |> Expect.equal "3068"
            , test "example" <| \_ -> example1 |> part2 100 "1000000000000" |> Expect.equal "1514285714288"
            , test "input" <| \_ -> input |> part2 5000 "1000000000000" |> Expect.equal "1586627906921"
            ]
        ]

type alias Point = (Int, Int)

type alias Rock = List Point
type alias Field = Set Point

emptyField: Field
emptyField = Set.empty

dropOne: Field -> String -> Rock -> (Field, String, Int)
dropOne field jets rock =
    let
        hitsField f r =
            r
            |> L.any (\(i,j) ->
                i < 0
                || j < 0
                || j > 6
                || Set.member (i,j) f
            )

        pushRock r jet f =
            let
                r2 = r |> L.map (\(i,j) ->
                    case jet of
                        '<' -> (i, j-1)
                        '>' -> (i, j+1)
                        _ -> Debug.todo "bad jet"
                    )
            in
            if hitsField f r2
            then r
            else r2

        dropRock r f =
            let
                r2 = r |> L.map (\(i,j) -> (i-1, j))
            in
            if hitsField f r2
            then Nothing
            else Just r2


        pasteRock f r =
            Set.union f (r |> Set.fromList)

        step : (Field, Rock) -> Char -> (Field, Maybe Rock)
        step (f,r) jet =
            let
                --_ = jet |> D.log "step"
                r2 = pushRock
                    (r
                        --|>D.log "r"
                    )
                    (jet
                        --|>D.log "j"
                    ) f
                    --|>D.log "r2"
                r3 = dropRock r2 f
                    --|>D.log "r3"
            in
            case r3 of
                Nothing -> (pasteRock f r2, Nothing)
                _ -> (f, r3)

        dropFully : Field -> Rock -> String -> Int -> (Field, String, Int)
        dropFully f r js count =
            case S.uncons js of
                Just (jet, js2) ->
                    let
                        --_ = (jet,js2)|>D.log "uncons"
                        (f2, mr2) = step (f, r) jet
                    in
                    case mr2 of
                        Nothing -> (f2, js2, count)
                        Just r2 -> dropFully f2 r2 js2 (count+1)
                Nothing -> D.todo "out of jets"
    in
    dropFully field rock jets 0
    --|> Debug.log "dropFully"

towerHeight: Field -> Int
towerHeight field =
    case field
        |> Set.toList
        |> LE.maximumBy (\(i,j)->i) of
        Nothing -> 0
        Just (i,j) -> i + 1

dropNew: Field -> String -> Int -> (Field, String, Int)
dropNew field jets rockIdx =
    let
        h = towerHeight field
        shape = LE.getAt (modBy (L.length rockShapes) rockIdx) rockShapes |> fromJust
        rock = shape |> L.map (\(i,j) -> (i+h+3,j))
    in
    dropOne field jets rock

cleanupField: Field -> Field
cleanupField field =
    let
        h = towerHeight field
        rows =
            field
            |> Set.toList
            |> LE.groupWhile (\(i1,j1) (i2,j2) -> i1==i2)
            |> L.map (\(p,ps)->p::ps)
            --|>D.log "rows"
        --_ = rows |> L.map (L.length) |> D.log "rows"
        fullRow =
            rows
            |> L.reverse
            |> LE.find (\row -> L.length row == 7)
    in
    case fullRow of
        Nothing -> field
        Just ((fullI,_)::_) ->
            field
            --|> D.log "before"
            |> Set.filter (\(i,j) -> i >= fullI)
            --|> D.log "filtered"
        Just [] -> Debug.todo "bad row"

asInt i =
    case Integer.toString i |> String.toInt of
        Nothing -> D.todo "not int"
        Just n -> if n < 10000000 then n
            else D.todo "too big"


part2 testIter iterStr input =
    let
        iter = Integer.fromString iterStr |> fromJust
        jets = String.repeat 10 input
        refill js =
            if S.length js < 1000 then
                js ++ jets
            else
                js
        rockIndexes = L.range 0 (testIter-1)
        (field, _, (counts,deltas)) =
            rockIndexes
            |> L.foldl (\t (f,js,(cnts,ds)) ->
                let
                    (f2,js2,cnt) = dropNew f (refill js) t
                    delta = towerHeight f2 - towerHeight f
                    f3 = cleanupField f2
                in
                (f3,js2,(cnts ++ [cnt],ds++[delta]))) (emptyField, jets, ([],[]))
        --_ = field |> D.log "final"
        --_ = counts |> D.log "counts"
        --_ = deltas |> D.log "deltas"

        cycleLength =
            case
                L.range 10 testIter
                |> L.reverse
                |> LE.find (\length ->
                    let
                        groups =
                            deltas
                            |> L.reverse
                            |> LE.groupsOf length
                    in
                        case groups of
                            (last::previous::_) -> 
                                (
                                    last 
                                    --|> D.log "last"
                                )

                                ==
                                (
                                    previous
                                    --|> D.log "prev"
                                )
                            _ -> False
                )
            of
                Nothing -> Debug.todo "no cycle found"
                Just n -> n |> Debug.log "cycleLength"

        cycle = deltas
            |> L.reverse
            |> L.take cycleLength
            |> L.reverse
            --|> D.log "cycle"

        beforeCycle =
            deltas
            |> L.reverse
            |> L.drop cycleLength
            |> L.reverse
            --|> D.log "before"

        _ = L.length beforeCycle |> D.log "lengthBefore"

        heightBefore = beforeCycle |> L.foldl (+) 0
            |> D.log "heihtBefore"

        cycleHeight = cycle |> L.foldl (+) 0
            |> D.log "cycleHeight"

        (cycleCount, lastCycleLength) = (Integer.divmod
                (Integer.sub iter (Integer.fromInt (L.length beforeCycle)))
                (Integer.fromInt cycleLength)
            )
            |> fromJust

        _ = cycleCount |> Integer.toString |> D.log "cycleCount"
        _ = lastCycleLength |> Integer.toString |> D.log "lastCycleLength"

        lastCycle =
            cycle
            |> L.take (lastCycleLength |> asInt)
            --|> D.log "lastCycle"

        lastCycleHeight = lastCycle |> L.foldl (+) 0
            |> D.log "lastCycleHeight"

    in
    (Integer.add
        (Integer.add
            (Integer.fromInt heightBefore)
            (Integer.mul cycleCount (Integer.fromInt cycleHeight))
        )
        (Integer.fromInt lastCycleHeight))
    |> Integer.toString
