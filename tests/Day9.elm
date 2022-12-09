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
                    oneMove [ ( 0, 0 ), ( 0, 0 ) ] "R 4"
                        |> Expect.equal
                            [ [ ( 0, 0 ), ( 0, 0 ) ]
                            , [ ( 1, 0 ), ( 0, 0 ) ]
                            , [ ( 2, 0 ), ( 1, 0 ) ]
                            , [ ( 3, 0 ), ( 2, 0 ) ]
                            , [ ( 4, 0 ), ( 3, 0 ) ]
                            ]
            , test "move up" <|
                \_ ->
                    oneMove [ ( 0, 0 ), ( 0, 0 ) ] "U 4"
                        |> Expect.equal
                            [ [ ( 0, 0 ), ( 0, 0 ) ]
                            , [ ( 0, 1 ), ( 0, 0 ) ]
                            , [ ( 0, 2 ), ( 0, 1 ) ]
                            , [ ( 0, 3 ), ( 0, 2 ) ]
                            , [ ( 0, 4 ), ( 0, 3 ) ]
                            ]
            , test "allMoves" <|
                \_ ->
                    example1
                        |> allMoves 2
                        |> Expect.equal
                            [ [ ( 0, 0 ), ( 0, 0 ) ]
                            , [ ( 1, 0 ), ( 0, 0 ) ]
                            , [ ( 2, 0 ), ( 1, 0 ) ]
                            , [ ( 3, 0 ), ( 2, 0 ) ]
                            , [ ( 4, 0 ), ( 3, 0 ) ]
                            , [ ( 4, 1 ), ( 3, 0 ) ]
                            , [ ( 4, 2 ), ( 4, 1 ) ]
                            , [ ( 4, 3 ), ( 4, 2 ) ]
                            , [ ( 4, 4 ), ( 4, 3 ) ]
                            , [ ( 3, 4 ), ( 4, 3 ) ]
                            , [ ( 2, 4 ), ( 3, 4 ) ]
                            , [ ( 1, 4 ), ( 2, 4 ) ]
                            , [ ( 1, 3 ), ( 2, 4 ) ]
                            , [ ( 2, 3 ), ( 2, 4 ) ]
                            , [ ( 3, 3 ), ( 2, 4 ) ]
                            , [ ( 4, 3 ), ( 3, 3 ) ]
                            , [ ( 5, 3 ), ( 4, 3 ) ]
                            , [ ( 5, 2 ), ( 4, 3 ) ]
                            , [ ( 4, 2 ), ( 4, 3 ) ]
                            , [ ( 3, 2 ), ( 4, 3 ) ]
                            , [ ( 2, 2 ), ( 3, 2 ) ]
                            , [ ( 1, 2 ), ( 2, 2 ) ]
                            , [ ( 0, 2 ), ( 1, 2 ) ]
                            , [ ( 1, 2 ), ( 1, 2 ) ]
                            , [ ( 2, 2 ), ( 1, 2 ) ]
                            ]
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 13
            , test "input" <| \_ -> input |> part1 |> Expect.equal 6212
            ]
        , describe "part 2"
            [ test "move right" <|
                \_ ->
                    oneMove [ ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) ] "R 4"
                        |> Expect.equal
                            [ [ ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) ]
                            , [ ( 1, 0 ), ( 0, 0 ), ( 0, 0 ) ]
                            , [ ( 2, 0 ), ( 1, 0 ), ( 0, 0 ) ]
                            , [ ( 3, 0 ), ( 2, 0 ), ( 1, 0 ) ]
                            , [ ( 4, 0 ), ( 3, 0 ), ( 2, 0 ) ]
                            ]
            , test "example1" <| \_ -> example1 |> part2 |> Expect.equal 1
            , test "example2" <| \_ -> example2 |> part2 |> Expect.equal 36
            , test "input" <| \_ -> input |> part2 |> Expect.equal 2522
            ]
        ]


type alias Rope =
    List ( Int, Int )


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


step : Step -> Rope -> Rope
step ( dx, dy ) state =
    case state of
        ( hx, hy ) :: tail ->
            (( hx + dx, hy + dy ) :: tail)
                |> LE.scanl1 catchup

        _ ->
            Debug.todo "bad rope"


oneMove : Rope -> String -> List Rope
oneMove state move =
    expandMove move
        |> LE.scanl step state


allMoves : Int -> String -> List Rope
allMoves ropeLength input =
    let
        steps =
            expandMoves input

        initial =
            L.repeat ropeLength ( 0, 0 )
    in
    LE.scanl step initial steps


tailPositionCount ropeLength input =
    allMoves ropeLength input
        |> L.map (LE.last >> fromJust)
        |> Set.fromList
        |> Set.size


part1 : String -> Int
part1 =
    tailPositionCount 2


part2 : String -> Int
part2 =
    tailPositionCount 10
