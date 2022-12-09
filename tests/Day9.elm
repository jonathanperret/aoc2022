module Day9 exposing (..)

import AocUtil exposing (..)
import Day9Input exposing (input)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set
import String.Extra
import Test exposing (..)
import Tuple
import String as S
import List as L
import Debug as D
import Array exposing (Array)


example1 = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""

example2 = """R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"""


suite : Test
suite =
    describe "day 9"
        [ describe "part 1"
            [
              test "move right" <| \_ -> applyMove {head=(0, 0), tails=[(0, 0)]} "R 4"
                |> Expect.equal     [{ head = (0,0), tails = [(0,0)] },{ head = (1,0), tails = [(0,0)] },{ head = (2,0), tails = [(1,0)] },{ head = (3,0), tails = [(2,0)] },{ head = (4,0), tails = [(3,0)] }]
            , test "move up" <| \_ -> applyMove {head=(4, 0), tails=[(3, 0)]} "U 4"
                |> Expect.equal     [{ head = (4,0), tails = [(3,0)] },{ head = (4,1), tails = [(3,0)] },{ head = (4,2), tails = [(4,1)] },{ head = (4,3), tails = [(4,2)] },{ head = (4,4), tails = [(4,3)] }]
            , test "allMoves" <| \_ -> example1 |> allMoves 1 |> Expect.equal     [{ head = (0,0), tails = [(0,0)] },
                                        { head = (0,0), tails = [(0,0)] },
                                        { head = (1,0), tails = [(0,0)] },
                                        { head = (2,0), tails = [(1,0)] },
                                        { head = (3,0), tails = [(2,0)] },
                                        { head = (4,0), tails = [(3,0)] },
                                        { head = (4,0), tails = [(3,0)] },
                                        { head = (4,1), tails = [(3,0)] },
                                        { head = (4,2), tails = [(4,1)] },
                                        { head = (4,3), tails = [(4,2)] },
                                        { head = (4,4), tails = [(4,3)] },
                                        { head = (4,4), tails = [(4,3)] },
                                        { head = (3,4), tails = [(4,3)] },
                                        { head = (2,4), tails = [(3,4)] },
                                        { head = (1,4), tails = [(2,4)] },
                                        { head = (1,4), tails = [(2,4)] },
                                        { head = (1,3), tails = [(2,4)] },
                                        { head = (1,3), tails = [(2,4)] },
                                        { head = (2,3), tails = [(2,4)] },
                                        { head = (3,3), tails = [(2,4)] },
                                        { head = (4,3), tails = [(3,3)] },
                                        { head = (5,3), tails = [(4,3)] },
                                        { head = (5,3), tails = [(4,3)] },
                                        { head = (5,2), tails = [(4,3)] },
                                        { head = (5,2), tails = [(4,3)] },
                                        { head = (4,2), tails = [(4,3)] },
                                        { head = (3,2), tails = [(4,3)] },
                                        { head = (2,2), tails = [(3,2)] },
                                        { head = (1,2), tails = [(2,2)] },
                                        { head = (0,2), tails = [(1,2)] },
                                        { head = (0,2), tails = [(1,2)] },
                                        { head = (1,2), tails = [(1,2)] },
                                        { head = (2,2), tails = [(1,2)] }]
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 13
            , test "input" <| \_ -> input |> part1 |> Expect.equal 6212
            ]
        , describe "part 2"
            [
              test "move right" <| \_ -> applyMove {head=(0, 0), tails=[(0, 0),(0,0)]} "R 4"
                |> Expect.equal  [{ head = (0,0), tails = [(0,0),(0,0)] },
                                     { head = (1,0), tails = [(0,0),(0,0)] },
                                     { head = (2,0), tails = [(1,0),(0,0)] },
                                     { head = (3,0), tails = [(2,0),(1,0)] },
                                     { head = (4,0), tails = [(3,0),(2,0)] }]
            , test "example" <| \_ -> example1 |> part2 |> Expect.equal 1
            , test "example2" <| \_ -> example2 |> part2 |> Expect.equal 36
            , test "input" <| \_ -> input |> part2 |> Expect.equal 2522
            ]
        ]

allMoves length input =
    let
        moves = String.lines input
        initial = [{head=(0,0), tails=L.repeat length (0,0)}]
    in
    LE.scanl (\m ss -> applyMove (LE.last ss |> fromJust) m) initial moves
    |> L.concat

catchup (hx, hy) (tx,ty) =
   (
    if hx > tx+1 || (hx>tx && (hy > ty+1 || hy < ty-1)) then
        tx + 1
    else if hx < tx-1 || (hx<tx && (hy > ty+1 || hy < ty-1)) then
        tx - 1
    else
        tx
    ,
    if hy > ty+1 || (hy>ty && (hx > tx+1 || hx < tx-1))  then
        ty + 1
    else if hy < ty-1 || (hy<ty && (hx > tx+1 || hx < tx-1))   then
        ty - 1
    else
        ty
    )

applyMove state move =
    let
        (dir, rstr) = case move |> String.words of
            [a,b]->(a,b)
            _ -> Debug.todo "bad move"

        repeats = rstr |> String.toInt |> fromJust

        (dx, dy) = case dir of
            "R" -> (1,0)
            "L" -> (-1,0)
            "U" -> (0,1)
            "D" -> (0,-1)
            _ -> Debug.todo "bad dir"

        doMove s =
            let
                (hx, hy) = s.head

                (hx2, hy2) = (hx+dx, hy+dy)

                newtails =
                    s.tails
                    |> LE.mapAccuml (\prev t ->
                        let
                            n = catchup prev t
                        in ( n, n )
                    )
                    (hx2, hy2)
                    |> Tuple.second

                news = { head=(hx2, hy2), tails=newtails }
            in
            news


    in
    L.range 1 repeats
    |> LE.scanl (\_ s -> doMove s) state

moveRope length input =
    let
        states = allMoves length input
    in
    states
    |> L.map (\s -> s.tails |> LE.last |> fromJust)
    |> LE.unique
    |> L.length


part1 input =
    moveRope 1 input

part2: String -> Int
part2 = moveRope 9
