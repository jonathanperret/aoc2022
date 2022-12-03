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
                 test "foundInSacks" <| \_ -> (foundInSacks example1 'r') |> Expect.equal 4
                ,test "example" <| \_ -> example1 |> part2 |> Expect.equal 70
                ,skip <| test "input" <| \_ -> input |> part2 |> Expect.equal 11063
            ]
        ]

part2: String -> Int
part2 sackStrs =
    let
        items = sackStrs
            |> String.toList
            |> Set.fromList
            |> Set.toList
            |> Debug.log "items"
        stats = items
            |> List.map (\item -> (item, foundInSacks sackStrs item))
            |> Debug.log "stats"
        stats3 = stats
            |> List.filter (\(item,n) -> n == 3)
            |> Debug.log "stats3"
        prios = stats3
            |> List.map (\(item,n) -> item)
            |> List.map itemPriority
            |> Debug.log "prios"
    in List.sum prios

itemPriority: Char -> Int
itemPriority item =
    case Char.isLower item of
        True -> (Char.toCode item) - (Char.toCode 'a') + 1
        False -> (Char.toCode item) - (Char.toCode 'A') + 27

foundInSacks: String -> Char -> Int
foundInSacks sackStrs item =
    let
        lines = sackStrs |> String.lines
        sacks = lines |> List.filter (String.any (\c -> c == item))

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
