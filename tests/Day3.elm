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
                ,test "input" <| \_ -> input |> part1 |> Expect.equal 8105
            ]
            ,describe "part 2"
            [
                 test "foundInSacks" <| \_ -> (foundInSacks (String.lines example1) 'r') |> Expect.equal 4
                ,test "commonItemPrio" <| \_ -> (commonItemPrio (String.lines example1 |> List.take 3)) |> Expect.equal 18
                ,test "example" <| \_ -> example1 |> part2 |> Expect.equal 70
                ,test "input" <| \_ -> input |> part2 |> Expect.equal 2363
            ]
        ]

part2: String -> Int
part2 input =
    let
        lines = String.lines input

        (_, result) = next3Sacks (lines, 0)
    in
        result

next3Sacks: (List String, Int) -> (List String, Int)
next3Sacks (lines, total) =
    let
        first3 = List.take 3 lines
        rest = List.drop 3 lines
    in
        case first3 of
            [] -> ([], total)
            _ -> next3Sacks (rest, total + commonItemPrio first3)

commonItemPrio: List String -> Int
commonItemPrio sackStrs =
    let
        items = sackStrs
            |> String.concat
            |> String.toList
            |> Set.fromList
            |> Set.toList
        stats = items
            |> List.map (\item -> (item, foundInSacks sackStrs item))
        stats3 = stats
            |> List.filter (\(item,n) -> n == 3)
        prios = stats3
            |> List.map (\(item,n) -> item)
            |> List.map itemPriority
    in List.sum prios

itemPriority: Char -> Int
itemPriority item =
    case Char.isLower item of
        True -> (Char.toCode item) - (Char.toCode 'a') + 1
        False -> (Char.toCode item) - (Char.toCode 'A') + 27

foundInSacks: List String -> Char -> Int
foundInSacks sackStrs item =
    let
        sacks = sackStrs |> List.filter (String.any (\c -> c == item))
    in List.length sacks

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

        prio = itemPriority commonItem
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
