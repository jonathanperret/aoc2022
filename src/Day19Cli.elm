port module Day19Cli exposing (..)

import Day19 exposing (..)
import Day19Input exposing (..)
import Json.Encode as Enc
import List as L
import Platform exposing (worker)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Process
import Stat
import String as S
import String.Interpolate exposing (interpolate)
import Task
import Time
import Debug as D
import Dict exposing (Dict)
import Array exposing (Array)
import AocUtil exposing (fromJust)
import List.Extra as LE

port jsonConsole : Enc.Value -> Cmd msg


consoleLog str args =
    interpolate str args |> Enc.string |> jsonConsole


type alias State =
    { bpidx: Int }


type Msg
    = Start


emptyState : State
emptyState =
    { bpidx = 0 }


start =
    Task.perform
        (\_ -> Start)
        (Process.sleep 10)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Start ->
            let
                blueprints = parse input

                maxGeodes =
                    blueprints
                    |> L.indexedMap (\i blueprint -> (i+1, findMaxGeodes blueprint))
            in
            ( state
            , consoleLog "{0}: {1} -> {2}"
              [ state.bpidx|>D.toString
              , maxGeodes|>D.toString
              , maxGeodes|>L.map (\(x,y)->x*y)|>L.sum|>D.toString
              ]
            )
--            let
--                (result, finalCache) = bestGeodes blueprint1
--                    { oreRobotCount=1, clayRobotCount=0, obsidianRobotCount=0 }
--                    { oreCount=0, clayCount=0, obsidianCount=0 }
--                    state.minutes
--                    emptyCache
--            in
--            ( state
--            , consoleLog "done: {0} cacheSize: {1}" [result|>S.fromInt, 
--                finalCache|>Array.toList|>L.map Dict.size|>L.map S.fromInt|>S.join " "
--            ]
--            )


main : Program (List String) State Msg
main =
    worker
        { init =
            \argv ->
                let
                    _ = argv
                    bpidx =
                        L.head argv |> Maybe.andThen String.toInt |> Maybe.withDefault 0
                in
                ( { emptyState | bpidx = bpidx |> Debug.log "bpidx" }
                , start
                )
        , subscriptions = \n -> Sub.none
        , update = update
        }
