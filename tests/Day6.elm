module Day6 exposing (..)

import AocUtil exposing (..)
import Day6Input exposing (input)
import Expect
import Fuzz
import List.Extra
import Set
import String.Extra
import Test exposing (..)
import Tuple


example1 =
    """mjqjpqmgbljsphdztnvjfqwrcgsmlb"""


suite : Test
suite =
    describe "day 6"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> Expect.equal 7
            , test "input" <| \_ -> input |> part1 |> Expect.equal 1262
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 19
            , test "input" <| \_ -> input |> part2 |> Expect.equal 3444
            ]
        ]

part1 : String -> Int
part1 = findMarker 4

part2 : String -> Int
part2 = findMarker 14

findMarker : Int -> String -> Int
findMarker size input =
    input
    |> String.toList
    |> List.Extra.groupsOfWithStep size 1
    |> List.Extra.findIndex List.Extra.allDifferent
    |> fromJust
    |> (+) size
