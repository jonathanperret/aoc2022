module Day19Tests exposing (..)

import AocUtil exposing (..)
import Day19Input exposing (..)
import Day19 exposing (..)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set exposing (Set)
import String.Extra
import Test exposing (..)
import Tuple
import String as S
import List as L
import Debug as D
import Array exposing (Array)
import Regex
import AStar.Generalised exposing (findPath)

import Dict exposing (Dict)
import Set exposing (Set)

suite : Test
suite =
    describe "day 19"
        [ describe "part 1"
            [ test "bestGeodes0" <| \_ ->
                bestGeodes blueprint1 emptyTeam emptyStorage 0 emptyCache |> Tuple.first
                |> Expect.equal 0

            , test "bestGeodes1" <| \_ ->
                bestGeodes blueprint1
                    { oreRobotCount=1, clayRobotCount=4, obsidianRobotCount=2 }
                    { oreCount=5, clayCount=37, obsidianCount=6 }
                    1
                    emptyCache |> Tuple.first
                |> Expect.equal 0

            , test "bestGeodes2" <| \_ ->
                bestGeodes blueprint1
                    { oreRobotCount=1, clayRobotCount=4, obsidianRobotCount=2 }
                    { oreCount=4, clayCount=33, obsidianCount=4 }
                    2
                    emptyCache |> Tuple.first
                |> Expect.equal 0

            , test "bestGeodes4" <| \_ ->
                bestGeodes blueprint1
                    { oreRobotCount=1, clayRobotCount=4, obsidianRobotCount=2 }
                    { oreCount=4, clayCount=25, obsidianCount=7 }
                    4
                    emptyCache |> Tuple.first
                |> Expect.equal 3

            , skip <| test "bestGeodes5" <| \_ ->
                bestGeodes blueprint1
                    { oreRobotCount=1, clayRobotCount=4, obsidianRobotCount=2 }
                    { oreCount=3, clayCount=21, obsidianCount=5 }
                    5
                    emptyCache |> Tuple.first
                |> Expect.equal 3

            , test "bestGeodes10" <| \_ ->
                bestGeodes blueprint1
                    { oreRobotCount=1, clayRobotCount=4, obsidianRobotCount=1 }
                    { oreCount=3, clayCount=15, obsidianCount=3 }
                    10
                    emptyCache |> Tuple.first
                |> Expect.equal 9

            , skip <| test "bestGeodes24" <| \_ ->
                bestGeodes blueprint1
                    { oreRobotCount=1, clayRobotCount=0, obsidianRobotCount=0 }
                    { oreCount=0, clayCount=0, obsidianCount=0 }
                    24
                    emptyCache |> Tuple.first
                |> Expect.equal 9

            , test "bestGeodesAStar 4" <| \_ ->
                bestGeodesAStar blueprint1
                    { oreRobotCount=1, clayRobotCount=4, obsidianRobotCount=2 }
                    { oreCount=4, clayCount=25, obsidianCount=7 }
                    4
                |> Expect.equal 3

            , test "bestGeodesAStar 5" <| \_ ->
                bestGeodesAStar blueprint1
                    { oreRobotCount=1, clayRobotCount=4, obsidianRobotCount=2 }
                    { oreCount=3, clayCount=21, obsidianCount=5 }
                    5
                |> Expect.equal 3

            , skip <| test "bestGeodesAStar 10" <| \_ ->
                bestGeodesAStar blueprint1
                    { oreRobotCount=1, clayRobotCount=4, obsidianRobotCount=1 }
                    { oreCount=3, clayCount=15, obsidianCount=3 }
                    10
                |> Expect.equal 9

--            , test "waysTo 0" <| \_ ->
--                nextStates blueprint1
--                    ([((
--                    { oreRobotCount=1, clayRobotCount=4, obsidianRobotCount=2 }
--                    { oreCount=3, clayCount=21, obsidianCount=5 }
--                    5
--                |> Expect.equal 3

            , skip <| test "qualityLevel" <| \_ -> example1 |> parse |> L.head |> fromJust |> qualityLevel |> Expect.equal 9

            , skip <| test "qualityLevel 2" <| \_ -> example1 |> parse |> L.drop 1 |> L.head |> fromJust |> qualityLevel |> Expect.equal 12

            , skip <| test "example" <| \_ -> example1 |> part1 |> Expect.equal 33
            , skip <| test "input" <| \_ -> input |> part1 |> Expect.equal 0
            ]
        , skip <| describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 0
            , skip <| test "input" <| \_ -> input |> part2 |> Expect.equal 0
            ]
        ]
