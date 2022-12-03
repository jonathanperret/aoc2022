module Day3 exposing (..)

import Day3Input exposing (input)
import Expect
import Fuzz
import Test exposing (..)
import Set

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
        [
            describe "part 1"
            [
                test "commonPriority" <| \_ -> "vJrwpWtwJgWrhcsFMMfFFhFp" |> commonPriority |>
                    Expect.equal 16
                ,test "example" <| \_ -> example1 |> part1 |> Expect.equal 157
                ,test "input" <| \_ -> input |> part1 |> Expect.equal 11063
            ]
        ]

commonPriority: String -> Int
commonPriority sackstr =
    let
        items = String.toList sackstr
        left = List.take ((List.length items) // 2) items
            --|> Debug.log "left"
        right = List.drop ((List.length items) // 2) items
            --|> Debug.log "right"
        leftSet = Set.fromList left
            --|>Debug.log "leftSet"
        rightSet = Set.fromList right
            --|>Debug.log "rightSet"

        common = Set.intersect leftSet rightSet
            --|> Debug.log "common"

        commonItem = (Set.toList common |> List.head |> \i -> case i of
                Just item -> item
                Nothing -> Debug.todo "no common item")
             --|> Debug.log "commonItem"

        prio = case Char.isLower commonItem of
            True -> (Char.toCode commonItem) - (Char.toCode 'a') + 1
            False -> (Char.toCode commonItem) - (Char.toCode 'A') + 27
    in
        prio


part1 : String -> Int
part1 input =
    let
        lines =
            String.lines input
    in
    List.map commonPriority lines
    |> List.sum
