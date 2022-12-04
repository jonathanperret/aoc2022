module Day3 exposing (..)

import AocUtil exposing (..)
import Day3Input exposing (input)
import Expect
import Fuzz
import List.Extra
import Set
import String.Extra
import Test exposing (..)


example1 =
    """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""


suite : Test
suite =
    describe "day 3"
        [ describe "part 1"
            [ test "commonItemPrio" <| \_ -> commonItemPrio (String.lines example1 |> List.take 3) |> Expect.equal 18
            , test "commonPriority" <| \_ -> "vJrwpWtwJgWrhcsFMMfFFhFp" |> halvesCommonPriority |> Expect.equal 16
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 157
            , test "input" <| \_ -> input |> part1 |> Expect.equal 8105
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 70
            , test "input" <| \_ -> input |> part2 |> Expect.equal 2363
            ]
        ]


commonItemPrio : List String -> Int
commonItemPrio sackStrs =
    let
        sets =
            sackStrs |> List.map (String.toList >> Set.fromList)

        common =
            sets |> List.Extra.foldl1 Set.intersect |> fromJust
    in
    common |> Set.toList |> List.map itemPriority |> List.sum


itemPriority : Char -> Int
itemPriority item =
    case Char.isLower item of
        True ->
            Char.toCode item - Char.toCode 'a' + 1

        False ->
            Char.toCode item - Char.toCode 'A' + 27


halvesCommonPriority : String -> Int
halvesCommonPriority sackstr =
    String.Extra.break (String.length sackstr // 2) sackstr
        |> commonItemPrio


part1 : String -> Int
part1 input =
    let
        lines =
            String.lines input
    in
    List.map halvesCommonPriority lines
        |> List.sum


part2 : String -> Int
part2 input =
    let
        lines =
            String.lines input

        groups =
            List.Extra.groupsOf 3 lines
    in
    groups |> List.map commonItemPrio |> List.sum
