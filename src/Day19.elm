module Day19 exposing (..)

import AocUtil exposing (..)
import Array exposing (Array)
import Day19Input exposing (..)
import Debug as D
import Dict exposing (Dict)
import Expect
import Fuzz
import List as L
import List.Extra as LE
import Regex
import Set exposing (Set)
import String as S
import String.Extra
import Tuple


type alias Team =
    { oreRobotCount : Int
    , clayRobotCount : Int
    , obsidianRobotCount : Int
    }


type alias Storage =
    { oreCount : Int
    , clayCount : Int
    , obsidianCount : Int
    }


makeOptions : Blueprint -> Storage -> Team -> List ( Storage, Team, Int )
makeOptions blueprint storage team =
    [ ( { storage
            | oreCount = storage.oreCount - blueprint.geodeOreCost
            , obsidianCount = storage.obsidianCount - blueprint.geodeObsidianCost
        }
      , team
      , 1
      )
    , ( { storage
            | oreCount = storage.oreCount - blueprint.obsidianOreCost
            , clayCount = storage.clayCount - blueprint.obsidianClayCost
        }
      , { team | obsidianRobotCount = team.obsidianRobotCount + 1 }
      , 0
      )
    , ( { storage | oreCount = storage.oreCount - blueprint.clayOreCost }
      , { team | clayRobotCount = team.clayRobotCount + 1 }
      , 0
      )
    , ( { storage | oreCount = storage.oreCount - blueprint.oreOreCost }
      , { team | oreRobotCount = team.oreRobotCount + 1 }
      , 0
      )
    , ( storage
      , team
      , 0
      )
    ]
        |> L.filter (\( st, tm, _ ) -> isOptionValid st tm)


isOptionValid { oreCount, clayCount, obsidianCount } _ =
    oreCount >= 0 && clayCount >= 0 && obsidianCount >= 0


oneStep : Blueprint -> Int -> ( Storage, Team, Int ) -> List ( Storage, Team, Int )
oneStep blueprint minute ( storage, team, geodes ) =
    let
        options : List ( Storage, Team, Int )
        options =
            makeOptions blueprint storage team
    in
    options
        |> L.map
            (\( st2, te, ge ) ->
                ( { st2
                    | oreCount = st2.oreCount + team.oreRobotCount
                    , clayCount = st2.clayCount + team.clayRobotCount
                    , obsidianCount = st2.obsidianCount + team.obsidianRobotCount
                  }
                , te
                , ge * (minute - 1) + geodes
                )
            )


stepAll : Blueprint -> Int -> List ( Storage, Team, Int ) -> List ( Storage, Team, Int )
stepAll blueprint minute states =
    states
        |> L.concatMap (oneStep blueprint minute)


findMaxGeodes : Int -> Blueprint -> Int
findMaxGeodes minutes blueprint =
    let
        states0 =
            [ ( { oreCount = 0, clayCount = 0, obsidianCount = 0 }
              , { oreRobotCount = 1, clayRobotCount = 0, obsidianRobotCount = 0 }
              , 0
              )
            ]

        finalStates =
            L.range 1 minutes
                |> L.foldr
                    (\minute ss ->
                        let
                            newStates =
                                stepAll blueprint minute ss
                        in
                        newStates
                            |> L.sortBy
                                (\( st, te, ge ) ->
                                    100000 * ge + te.obsidianRobotCount * 1000 + te.clayRobotCount * 100 + te.oreRobotCount
                                )
                            |> L.reverse
                            |> L.take 10000
                    )
                    states0
    in
    finalStates |> L.map (\( _, _, ge ) -> ge) |> L.maximum |> fromJust


part1 input =
    let
        blueprints =
            parse input

        maxGeodes =
            blueprints
                |> L.indexedMap (\i blueprint -> ( i + 1, findMaxGeodes 24 blueprint ))
    in
    maxGeodes |> L.map (\( idx, max ) -> idx * max) |> L.sum


part2 input =
    let
        blueprints =
            parse input |> L.take 3

        maxGeodes =
            blueprints
                |> L.map (findMaxGeodes 32)
    in
    maxGeodes |> L.product
