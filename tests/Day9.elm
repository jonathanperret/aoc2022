module Day9 exposing (..)

import AocUtil exposing (..)
import Array exposing (Array)
import Day9Input exposing (input)
import Debug as D
import Expect
import Fuzz
import List as L
import List.Extra as LE
import Set
import String as S
import String.Extra
import Test exposing (..)
import Tuple


example1 =
    """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""


example2 =
    """R 5
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
            [ test "move right" <|
                \_ ->
                    oneMove { head = ( 0, 0 ), tail = [ ( 0, 0 ) ] } "R 4"
                        |> Expect.equal
                            [ { head = ( 0, 0 ), tail = [ ( 0, 0 ) ] }
                            , { head = ( 1, 0 ), tail = [ ( 0, 0 ) ] }
                            , { head = ( 2, 0 ), tail = [ ( 1, 0 ) ] }
                            , { head = ( 3, 0 ), tail = [ ( 2, 0 ) ] }
                            , { head = ( 4, 0 ), tail = [ ( 3, 0 ) ] }
                            ]
            , test "move up" <|
                \_ ->
                    oneMove { head = ( 4, 0 ), tail = [ ( 3, 0 ) ] } "U 4"
                        |> Expect.equal
                            [ { head = ( 4, 0 ), tail = [ ( 3, 0 ) ] }
                            , { head = ( 4, 1 ), tail = [ ( 3, 0 ) ] }
                            , { head = ( 4, 2 ), tail = [ ( 4, 1 ) ] }
                            , { head = ( 4, 3 ), tail = [ ( 4, 2 ) ] }
                            , { head = ( 4, 4 ), tail = [ ( 4, 3 ) ] }
                            ]
            , test "allMoves" <|
                \_ ->
                    example1
                        |> allMoves 1
                        |> Expect.equal
                            [ { head = ( 0, 0 ), tail = [ ( 0, 0 ) ] }
                            , { head = ( 1, 0 ), tail = [ ( 0, 0 ) ] }
                            , { head = ( 2, 0 ), tail = [ ( 1, 0 ) ] }
                            , { head = ( 3, 0 ), tail = [ ( 2, 0 ) ] }
                            , { head = ( 4, 0 ), tail = [ ( 3, 0 ) ] }
                            , { head = ( 4, 1 ), tail = [ ( 3, 0 ) ] }
                            , { head = ( 4, 2 ), tail = [ ( 4, 1 ) ] }
                            , { head = ( 4, 3 ), tail = [ ( 4, 2 ) ] }
                            , { head = ( 4, 4 ), tail = [ ( 4, 3 ) ] }
                            , { head = ( 3, 4 ), tail = [ ( 4, 3 ) ] }
                            , { head = ( 2, 4 ), tail = [ ( 3, 4 ) ] }
                            , { head = ( 1, 4 ), tail = [ ( 2, 4 ) ] }
                            , { head = ( 1, 3 ), tail = [ ( 2, 4 ) ] }
                            , { head = ( 2, 3 ), tail = [ ( 2, 4 ) ] }
                            , { head = ( 3, 3 ), tail = [ ( 2, 4 ) ] }
                            , { head = ( 4, 3 ), tail = [ ( 3, 3 ) ] }
                            , { head = ( 5, 3 ), tail = [ ( 4, 3 ) ] }
                            , { head = ( 5, 2 ), tail = [ ( 4, 3 ) ] }
                            , { head = ( 4, 2 ), tail = [ ( 4, 3 ) ] }
                            , { head = ( 3, 2 ), tail = [ ( 4, 3 ) ] }
                            , { head = ( 2, 2 ), tail = [ ( 3, 2 ) ] }
                            , { head = ( 1, 2 ), tail = [ ( 2, 2 ) ] }
                            , { head = ( 0, 2 ), tail = [ ( 1, 2 ) ] }
                            , { head = ( 1, 2 ), tail = [ ( 1, 2 ) ] }
                            , { head = ( 2, 2 ), tail = [ ( 1, 2 ) ] }
                            ]
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 13
            , test "input" <| \_ -> input |> part1 |> Expect.equal 6212
            ]
        , describe "part 2"
            [ test "move right" <|
                \_ ->
                    oneMove { head = ( 0, 0 ), tail = [ ( 0, 0 ), ( 0, 0 ) ] } "R 4"
                        |> Expect.equal
                            [ { head = ( 0, 0 ), tail = [ ( 0, 0 ), ( 0, 0 ) ] }
                            , { head = ( 1, 0 ), tail = [ ( 0, 0 ), ( 0, 0 ) ] }
                            , { head = ( 2, 0 ), tail = [ ( 1, 0 ), ( 0, 0 ) ] }
                            , { head = ( 3, 0 ), tail = [ ( 2, 0 ), ( 1, 0 ) ] }
                            , { head = ( 4, 0 ), tail = [ ( 3, 0 ), ( 2, 0 ) ] }
                            ]
            , test "example1" <| \_ -> example1 |> part2 |> Expect.equal 1
            , test "example2" <| \_ -> example2 |> part2 |> Expect.equal 36
            , test "input" <| \_ -> input |> part2 |> Expect.equal 2522
            ]
        ]


type alias State =
    { head : ( Int, Int ), tail : List ( Int, Int ) }


type alias Step =
    ( Int, Int )


expandMoves : String -> List Step
expandMoves =
    String.lines >> L.concatMap expandMove


expandMove : String -> List Step
expandMove line =
    let
        ( dir, rstr ) =
            case line |> String.words of
                [ a, b ] ->
                    ( a, b )

                _ ->
                    Debug.todo "bad move"

        repeats =
            rstr |> String.toInt |> fromJust

        ( dx, dy ) =
            case dir of
                "R" ->
                    ( 1, 0 )

                "L" ->
                    ( -1, 0 )

                "U" ->
                    ( 0, 1 )

                "D" ->
                    ( 0, -1 )

                _ ->
                    Debug.todo "bad dir"
    in
    L.repeat repeats ( dx, dy )


catchup ( tx, ty ) ( hx, hy ) =
    ( if hx > tx + 1 || (hx > tx && (hy > ty + 1 || hy < ty - 1)) then
        tx + 1

      else if hx < tx - 1 || (hx < tx && (hy > ty + 1 || hy < ty - 1)) then
        tx - 1

      else
        tx
    , if hy > ty + 1 || (hy > ty && (hx > tx + 1 || hx < tx - 1)) then
        ty + 1

      else if hy < ty - 1 || (hy < ty && (hx > tx + 1 || hx < tx - 1)) then
        ty - 1

      else
        ty
    )


step : Step -> State -> State
step ( dx, dy ) state =
    let
        ( hx, hy ) =
            state.head

        ( hx2, hy2 ) =
            ( hx + dx, hy + dy )

        newtail =
            state.tail
                |> LE.scanl catchup ( hx2, hy2 )
                |> List.drop 1
    in
    { head = ( hx2, hy2 ), tail = newtail }


oneMove : State -> String -> List State
oneMove state move =
    let
        steps =
            expandMove move
    in
    LE.scanl step state steps


allMoves : Int -> String -> List State
allMoves tailLength input =
    let
        steps =
            expandMoves input

        initial =
            { head = ( 0, 0 ), tail = L.repeat tailLength ( 0, 0 ) }
    in
    LE.scanl (\m s -> step m s) initial steps


tailPositionCount tailLength input =
    allMoves tailLength input
        |> L.map (\s -> s.tail |> LE.last |> fromJust)
        |> Set.fromList
        |> Set.size


part1 : String -> Int
part1 =
    tailPositionCount 1


part2 : String -> Int
part2 =
    tailPositionCount 9
