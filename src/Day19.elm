module Day19 exposing (..)

import AocUtil exposing (..)
import Day19Input exposing (..)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set exposing (Set)
import String.Extra
import Tuple
import String as S
import List as L
import Debug as D
import Array exposing (Array)
import Regex
import AStar.Generalised exposing (findPath)

import Dict exposing (Dict)
import Set exposing (Set)
import Array exposing (Array)

type alias Team =
    { oreRobotCount: Int
    , clayRobotCount: Int
    , obsidianRobotCount: Int
    }

emptyTeam =
    { oreRobotCount= 0
    , clayRobotCount= 0
    , obsidianRobotCount= 0
    }

type alias Storage =
    { oreCount: Int
    , clayCount: Int
    , obsidianCount: Int
    }

emptyStorage=
    { oreCount= 0
    , clayCount= 0
    , obsidianCount= 0
    }

type alias CacheKey =
    { storage: Storage
    , team: Team
    , minutesRemaining: Int
    }

type alias ComparableCacheKey = ((Int,Int,Int), (Int,Int,Int))

type alias Cache = Array (Dict ComparableCacheKey Int)

toKey : CacheKey -> ComparableCacheKey
toKey { storage, team, minutesRemaining } =
    ( ( storage.oreCount, storage.clayCount, storage.obsidianCount )
    , ( team.oreRobotCount, team.clayRobotCount, team.obsidianRobotCount )
    )

emptyCache: Cache
emptyCache = Array.repeat 25 Dict.empty

type alias Result = Int

cacheInsert: CacheKey -> Result -> Cache -> Cache
cacheInsert key result cache =
    case Array.get key.minutesRemaining cache of
        Nothing -> D.todo "bad minute"
        Just dict -> Array.set key.minutesRemaining
            (Dict.insert (toKey key) (result 
                --|> D.log ("inserting " ++ D.toString key)
            ) dict) cache


cacheLookup key cache =
    case Array.get key.minutesRemaining cache of
        Nothing -> D.todo "bad minute"
        Just dict ->
            let
                found = Dict.get (toKey key) dict
            in
            case found of
                Just x -> Just x
                 --|> Debug.log ("found for " ++ Debug.toString key)
                Nothing -> Nothing

makeOptions: Blueprint -> Storage -> Team -> List (Storage, Team, Int)
makeOptions blueprint storage team =
    [
      ( { storage |
            oreCount = storage.oreCount - blueprint.geodeOreCost,
            obsidianCount = storage.obsidianCount - blueprint.geodeObsidianCost }
      , team
      , 1
      )
    , ( { storage |
            oreCount = storage.oreCount - blueprint.obsidianOreCost,
            clayCount = storage.clayCount - blueprint.obsidianClayCost }
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
    |> L.filter (\(st, tm, _) -> isOptionValid st tm)
    --|> L.take 2

isOptionValid { oreCount, clayCount, obsidianCount } { oreRobotCount, clayRobotCount, obsidianRobotCount } =
    oreCount >= 0 && clayCount >= 0 && obsidianCount >= 0
    -- && oreRobotCount < 3 && clayRobotCount < 6 && obsidianRobotCount < 4

oneStep: Blueprint -> Int -> (Storage, Team, Int) -> List (Storage, Team, Int)
oneStep blueprint minute (storage, team, geodes) =
    let
        options: List (Storage, Team, Int)
        options = makeOptions blueprint storage team
--        _ = case options |> L.filter (\(_,_,g)->g>0) |> L.take 4 of
--            [] -> []
--            l -> l |> D.log "opts"
    in
    options
    |> L.map (\(st2, te, ge) ->
        ({ st2 | oreCount = st2.oreCount+team.oreRobotCount,
                 clayCount = st2.clayCount+team.clayRobotCount,
                 obsidianCount = st2.obsidianCount + team.obsidianRobotCount},
         te, ge*(minute - 1) + geodes)
    )

stepAll: Blueprint -> Int -> List (Storage, Team, Int) -> List (Storage, Team, Int)
stepAll blueprint minute states =
    states
    |> L.concatMap (oneStep blueprint minute)
    --|> D.log "stepAll"

findMaxGeodes: Blueprint -> Int
findMaxGeodes blueprint =
    let
        states0 =
              [ ( { oreCount=0, clayCount=0, obsidianCount=0 }
                , { oreRobotCount=1, clayRobotCount=0, obsidianRobotCount=0 }
                , 0
                )
              ]

        lastMinute = 1

        finalStates =
            L.range lastMinute 24
            |> L.reverse
            |> L.foldl (\minute ss ->
                let
                    newStates = stepAll blueprint minute ss
                    _ = newStates
                        |> L.map (\(_,_,x)->x) |> LE.unique
                        |> D.log ((D.toString minute) ++ " " ++ (L.length newStates|>D.toString))

                    bestNewStates =
                        newStates
                        |> L.sortBy (\(st,te,ge) ->
                            100000*ge + te.obsidianRobotCount * 100 + te.clayRobotCount * 10 + te.oreRobotCount
                        )
                        |> L.drop (L.length newStates - 10000)
                in
                bestNewStates
            )
            states0
    in
    finalStates |> LE.maximumBy (\(_,_,ge) -> ge) |> fromJust |> (\(_,_,ge)->ge)

bestGeodes: Blueprint -> Team -> Storage -> Int -> Cache -> (Int, Cache)
bestGeodes blueprint team storage minutesRemaining cache =
    if minutesRemaining <= 1 then
        (0, cache)
    else if minutesRemaining == 2 then
        if storage.oreCount >= blueprint.geodeOreCost && storage.obsidianCount >= blueprint.geodeObsidianCost then
            (1, cache)
        else
            (0, cache)
    else
    let
        tryOption: (Storage, Team, Int) -> (List Int, Cache) -> (List Int, Cache)
        tryOption (optionStorage, optionTeam, newGeodeRobotCount) (resultsSoFar, cacheSoFar) =
            let
                storageAfterMining =
                    { oreCount=optionStorage.oreCount + team.oreRobotCount
                    , clayCount=optionStorage.clayCount + team.clayRobotCount
                    , obsidianCount=optionStorage.obsidianCount + team.obsidianRobotCount
                    }

                newGeodes = (minutesRemaining - 1) * newGeodeRobotCount

                cacheKey = { storage=storageAfterMining, team=optionTeam, minutesRemaining = minutesRemaining - 1 }

                (optionResult, cacheAfterOption) =
                    case cacheLookup cacheKey cacheSoFar of
                        Just cached -> (cached, cacheSoFar)
                        Nothing -> bestGeodes blueprint optionTeam storageAfterMining (minutesRemaining - 1) cacheSoFar
            in
            ( (optionResult + newGeodes) :: resultsSoFar,
              cacheAfterOption )

        options = makeOptions blueprint storage team

        (optionsResults, cacheAfterOptions) =
            options
            |> L.foldl tryOption ([], cache)

        newCacheKey = { storage=storage, team=team, minutesRemaining=minutesRemaining }

        bestResult = optionsResults |> L.maximum |> fromJust
        result = bestResult
    in
    ( result,
      cacheInsert newCacheKey result cacheAfterOptions |> (\c ->
          let
              _ = c |> Array.map Dict.size |> D.log "cache size"
          in c)

    )

type alias Node =
    ( (Int, Int)     -- minutes remaining, did build a geode robot
    , (Int, Int, Int) -- ore, clay, obsidian robots
    , (Int, Int, Int) -- ore, clay, obsidian stocks
    )


bestGeodesAStar: Blueprint -> Team -> Storage -> Int -> Int
bestGeodesAStar blueprint team storage minutesRemaining =
    let
        x=1
        endNode =
            ( (0, 0)
            , (0, 0, 0)
            , (0, 0, 0)
            )
        startNode =
            ( (minutesRemaining, 0)
            , (team.oreRobotCount, team.clayRobotCount, team.obsidianRobotCount)
            , (storage.oreCount, storage.clayCount, storage.obsidianCount)
            )

        moveFunc: Node -> Set Node
        moveFunc ((m, build), (oreR, clayR, obsR), (ore, clay, obs)) =
            if m==0 then
                Set.singleton endNode
            else
            let
                options =
                    [ ((oreR, clayR, obsR), (ore, clay, obs))
                    , ((oreR+1, clayR, obsR), (ore-blueprint.oreOreCost, clay, obs))
                    , ((oreR, clayR+1, obsR), (ore-blueprint.clayOreCost, clay, obs))
                    , ((oreR, clayR, obsR+1), (ore-blueprint.obsidianOreCost, clay-blueprint.obsidianClayCost, obs))
                    , ((oreR, clayR, obsR), (ore-blueprint.geodeOreCost, clay, obs-blueprint.geodeObsidianCost))
                    ]
                    |> L.filter (\(_, (o, c, ob)) -> o>=0 && c>=0 && ob >=0)
                    -- mine
                    |> L.map (\(rs, (o, c, ob)) -> (rs, (o+oreR, c+clayR, ob+obsR)))
            in
            options
            |> L.map (\(rs,(ore2, clay2, obs2)) -> ((m-1, if obs2-obsR < obs then 1 else 0), rs, (ore2, clay2, obs2)))
            |> Set.fromList


        costFunc: Node -> Node -> Float
        costFunc n1 (((m,build), _,_) as n2) =
            let
                cost =
                    if n1 == endNode then
                        if build == 0 then (m |> toFloat) else 0
                    else
                        D.todo "endNode only"
            in
            cost |> D.log ("cost of "++(D.toString n2))

        bestPath = findPath
            costFunc
            moveFunc
            startNode
            endNode
            |> fromJust
            |> ((::) startNode)
            |> D.log "bestPath"

        pathValue =
            bestPath
            |> L.map (\((m, build), _, _) ->
                m * build
            )
            |> D.log "values"
            |> L.sum
    in
    pathValue

initialTeam = { emptyTeam | oreRobotCount = 1 }

qualityLevel: Blueprint -> Result
qualityLevel blueprint =
    bestGeodes blueprint initialTeam emptyStorage 24 emptyCache
    |> (\(r,cache) ->
        let
          _ = cache |> Array.map Dict.size |> D.log "cache size"
        in
        r)

part1 input =
    let
        blueprints =
            input
            |> parse
    in
    blueprints
    |> L.map qualityLevel
    |> L.sum

part2: String -> Int
part2 input = 0
