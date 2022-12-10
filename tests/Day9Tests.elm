module Day9Tests exposing (..)

import AocUtil exposing (..)
import Array exposing (Array)
import Day9Input exposing (input)
import Debug as D
import Expect
import Fuzz
import List as L
import List.Extra as LE
import Set exposing (Set)
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
                    oneMove (makeInitial 2) "R 4"
                        |> Expect.equal
                            { rope = [ ( 4, 0 ), ( 3, 0 ) ]
                            , visited = Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]
                            }
            , test "move up" <|
                \_ ->
                    oneMove (makeInitial 2) "U 4"
                        |> Expect.equal
                            { rope = [ ( 0, 4 ), ( 0, 3 ) ]
                            , visited = Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]
                            }
            , test "allMoves" <|
                \_ ->
                    example1
                        |> allMoves 2
                        |> Expect.equal
                            { rope = [ ( 2, 2 ), ( 1, 2 ) ]
                            , visited = Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 1, 2 ), ( 2, 0 ), ( 2, 2 ), ( 2, 4 ), ( 3, 0 ), ( 3, 2 ), ( 3, 3 ), ( 3, 4 ), ( 4, 1 ), ( 4, 2 ), ( 4, 3 ) ]
                            }
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 13
            , test "input" <| \_ -> input |> part1 |> Expect.equal 6212
            ]
        , describe "part 2"
            [ test "move right" <|
                \_ ->
                    oneMove (makeInitial 3) "R 4"
                        |> Expect.equal
                            { rope = [ ( 4, 0 ), ( 3, 0 ), ( 2, 0 ) ]
                            , visited = Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
                            }
            , test "example1" <| \_ -> example1 |> part2 |> Expect.equal 1
            , test "example2" <| \_ -> example2 |> part2 |> Expect.equal 36
            , test "input" <| \_ -> input |> part2 |> Expect.equal 2522
            ]
        ]


type alias State =
    { rope : Rope, visited : Set ( Int, Int ) }


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


step : Step -> State -> State
step ( dx, dy ) state =
    case state.rope of
        ( hx, hy ) :: tail ->
            let
                newrope =
                    (( hx + dx, hy + dy ) :: tail)
                        |> LE.scanl1 catchup

                newtail =
                    LE.last newrope |> fromJust
            in
            { state | rope = newrope, visited = state.visited |> Set.insert newtail }

        _ ->
            Debug.todo "bad rope"


oneMove : State -> String -> State
oneMove state move =
    expandMove move
        |> L.foldl step state


makeInitial : Int -> State
makeInitial ropeLength =
    { rope = L.repeat ropeLength ( 0, 0 )
    , visited = Set.empty
    }


allMoves : Int -> String -> State
allMoves ropeLength input =
    let
        steps =
            expandMoves input

        initial =
            makeInitial ropeLength
    in
    L.foldl step initial steps


tailPositionCount ropeLength input =
    allMoves ropeLength input
        |> .visited
        |> Set.size


part1 : String -> Int
part1 =
    tailPositionCount 2


part2 : String -> Int
part2 =
    tailPositionCount 10
