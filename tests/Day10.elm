module Day10 exposing (..)

import AocUtil exposing (..)
import Day10Input exposing (input)
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


example1 =
    """noop
addx 3
addx -5"""

example2 = """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"""


suite : Test
suite =
    describe "day 10"
        [ describe "part 1"
            [
              test "noop" <| \_ ->{ startcycle=9, endcycle=10, x=42, prevx=41 } |> step "noop" |> Expect.equal { startcycle=10, endcycle=11, x=42, prevx=42 }
            , test "example2" <| \_ -> example2 |> part1 |> Expect.equal 13140
            , test "input" <| \_ -> input |> part1 |> Expect.equal 14720
            ]
        , describe "part 2"
            [ test "example2" <| \_ -> example2 |> part2 |> Expect.equal
                [ "##..##..##..##..##..##..##..##..##..##.."
                , "###...###...###...###...###...###...###."
                , "####....####....####....####....####...."
                , "#####.....#####.....#####.....#####....."
                , "######......######......######......####"
                , "#######.......#######.......#######....." ]
            , test "input" <| \_ -> input |> part2 |> Expect.equal 
                [ "####.####.###..###..###..####.####.####."
                , "#.......#.#..#.#..#.#..#.#.......#.#...."
                , "###....#..###..#..#.###..###....#..###.."
                , "#.....#...#..#.###..#..#.#.....#...#...."
                , "#....#....#..#.#....#..#.#....#....#...."
                , "#....####.###..#....###..#....####.#...."]
            ]
        ]


type alias State = { startcycle: Int, endcycle: Int, x: Int, prevx: Int }

step : String -> State -> State
step instr state =
    let
        (duration, dx) = case String.split " " instr of
            ["addx", argstr] -> (2, String.toInt argstr |> fromJust)
            _ -> (1, 0)

    in { state | startcycle=state.endcycle, endcycle = state.endcycle+duration, x=state.x+dx, prevx=state.x }


part1 input =
    let
        instrs =
            input
            |> S.lines
        initial = { startcycle=0, endcycle=0, x=1, prevx=1 }
        states = LE.scanl step initial instrs
        --|> Debug.log "states"

        interestingStates =
            states
            |> L.filter
                (\s ->
                    let
                        start = (s.startcycle + 20) // 40
                        end = (s.endcycle + 20) // 40
                        result = end > start

                        _ = ((s.startcycle,s.endcycle),(start,end,result))
                         --|>Debug.log "startend"
                    in result)
                    --|> Debug.log "states"
        values =
            interestingStates
            |> L.map (\s -> s.prevx * (s.endcycle - modBy 2 s.endcycle))
            |> Debug.log "values"
    in
    values |> L.sum

part2 : String -> List String
part2 input =
    let
        instrs =
            input
            |> S.lines
        initial = { startcycle=0, endcycle=0, x=1, prevx=1 }
        states = LE.scanl step initial instrs
                        |> L.drop 1
            --|> Debug.log "states"

        xvalues =
            L.range 0 239
            |> L.map (\c ->
                let
                    state =
                        states
                        |> LE.find (\s -> c >= s.startcycle && c < s.endcycle)
                in
                (c, state |> Maybe.map .prevx)
                )
            --|> Debug.log "xvalues"

        pixels =
            xvalues
            |> L.map (\(c,mx) ->
                let
                    col = modBy 40 c
                    x = mx |> Maybe.withDefault -100
                    hit = x == col || (x - 1) ==col || (x + 1) == col
                    _ = ((c,col),(x,hit))

                in if hit then "#" else "."
            )
            |> LE.groupsOf 40
            |> L.map (String.join "")
            --|> Debug.log "pixels"
    in
    pixels
