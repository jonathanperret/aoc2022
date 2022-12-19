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
                result1 = part1 input
                result2 = part2 input
            in
            ( state
            , consoleLog "{0}: part1={1} part2={2}"
              [ state.bpidx|>D.toString
              , result1|>D.toString
              , result2|>D.toString
              ]
            )


main : Program (List String) State Msg
main =
    worker
        { init =
            \argv ->
                let
                    _ = argv
                in
                ( emptyState
                , start
                )
        , subscriptions = \n -> Sub.none
        , update = update
        }
