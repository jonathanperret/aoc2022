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
        [ describe "part 1"
            [
              test "dropOne" <| \_ -> dropOne emptyField ">>><oooooo"
                [(3,2),(3,3),(3,4),(3,5)]
                |> Expect.equal
                (Set.fromList [(0,2),(0,3),(0,4),(0,5)], "oooooo")
            , test "dropOne2" <| \_ -> dropOne
                (Set.fromList [(0,2),(0,3),(0,4),(0,5)])
                "<><>oooo"
                ([(4,3),(5,2),(5,3),(5,4),(6,3)])
                |> Expect.equal
                (Set.fromList [(0,2),(0,3),(0,4),(0,5),
                    (1,3),(2,2),(2,3),(2,4),(3,3)], "oooo")
            , test "dropNew" <| \_ -> dropNew emptyField ">>><ooo"
                0
                |> Expect.equal (Set.fromList [(0,2),(0,3),(0,4),(0,5)], "ooo")
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 3068
            , test "input" <| \_ -> input |> part1 |> Expect.equal 3171
            ]
        , skip <| describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 0
            , skip <| test "input" <| \_ -> input |> part2 |> Expect.equal 0
            ]
        ]

type alias Point = (Int, Int)

type alias Rock = List Point
type alias Field = Set Point

emptyField: Field
emptyField = Set.empty

dropOne: Field -> String -> Rock -> (Field, String)
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

        dropFully : Field -> Rock -> String -> (Field, String)
        dropFully f r js =
            case S.uncons js of
                Just (jet, js2) ->
                    let
                        --_ = (jet,js2)|>D.log "uncons"
                        (f2, mr2) = step (f, r) jet
                    in
                    case mr2 of
                        Nothing -> (f2, js2)
                        Just r2 -> dropFully f2 r2 js2
                Nothing -> D.todo "out of jets"
    in
    dropFully field rock jets
    --|> Debug.log "dropFully"

towerHeight: Field -> Int
towerHeight field =
    case field
        |> Set.toList
        |> LE.maximumBy (\(i,j)->i) of
        Nothing -> 0
        Just (i,j) -> i + 1

dropNew: Field -> String -> Int -> (Field, String)
dropNew field jets rockIdx =
    let
        h = towerHeight field
        shape = LE.getAt (modBy (L.length rockShapes) rockIdx) rockShapes |> fromJust
        rock = shape |> L.map (\(i,j) -> (i+h+3,j))
    in
    dropOne field jets rock

part1 input =
    let
        jets = String.repeat 10 input
        refill js =
            if S.length js < 1000 then
                js ++ jets
            else
                js
        times = L.range 0 2021
        (field, _) =
            times
            |> L.foldl (\t (f,js) -> dropNew f (refill js) t) (emptyField, jets)
        _ = field
            ---|> D.log "final"
    in
    towerHeight field

part2: String -> Int
part2 input = 0
