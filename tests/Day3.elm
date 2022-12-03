module Day3 exposing (..)

import Day3Input exposing (input)
import Expect
import Fuzz
import List.Extra
import Set
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
            [ test "commonPriority" <|
                \_ ->
                    "vJrwpWtwJgWrhcsFMMfFFhFp"
                        |> commonPriority
                        |> Expect.equal 16
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 157
            , test "input" <| \_ -> input |> part1 |> Expect.equal 8105
            ]
        , describe "part 2"
            [ test "foundInSacks" <| \_ -> foundInSacks (String.lines example1) 'r' |> Expect.equal 4
            , test "commonItemPrio" <| \_ -> commonItemPrio (String.lines example1 |> List.take 3) |> Expect.equal 18
            , test "example" <| \_ -> example1 |> part2 |> Expect.equal 70
            , test "input" <| \_ -> input |> part2 |> Expect.equal 2363
            ]
        ]


part2 : String -> Int
part2 input =
    let
        lines =
            String.lines input

        groups =
            List.Extra.groupsOf 3 lines
    in
    groups |> List.map commonItemPrio |> List.sum


commonItemPrio : List String -> Int
commonItemPrio sackStrs =
    let
        items =
            sackStrs
                |> String.concat
                |> String.toList
                |> List.Extra.unique

        stats =
            items
                |> List.map (\item -> ( item, foundInSacks sackStrs item ))

        stats3 =
            stats
                |> List.filter (\( item, n ) -> n == 3)

        prios =
            stats3
                |> List.map (\( item, n ) -> item)
                |> List.map itemPriority
    in
    List.sum prios


itemPriority : Char -> Int
itemPriority item =
    case Char.isLower item of
        True ->
            Char.toCode item - Char.toCode 'a' + 1

        False ->
            Char.toCode item - Char.toCode 'A' + 27


foundInSacks : List String -> Char -> Int
foundInSacks sackStrs item =
    sackStrs
        |> List.Extra.count (String.any (\c -> c == item))


commonPriority : String -> Int
commonPriority sackstr =
    let
        items =
            String.toList sackstr

        ( left, right ) =
            List.Extra.splitAt (List.length items // 2) items

        leftSet =
            Set.fromList left

        rightSet =
            Set.fromList right

        common =
            Set.intersect leftSet rightSet

        commonItem =
            Set.toList common
                |> List.head
                |> Maybe.withDefault '_'
    in
    itemPriority commonItem


part1 : String -> Int
part1 input =
    let
        lines =
            String.lines input
    in
    List.map commonPriority lines
        |> List.sum
