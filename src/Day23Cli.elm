port module Day23Cli exposing (..)

import Day23 exposing (..)
import Day23Input exposing (..)
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
import Set exposing (Set)
import Array exposing (Array)
import List.Extra as LE

port jsonConsole : Enc.Value -> Cmd msg


consoleLog str args =
    interpolate str args |> Enc.string |> jsonConsole


type alias State =
    { interval: Int
    , map: Map
    , round: Int
    }


type Msg
    = Start


emptyState : State
emptyState =
    { interval=0
    , map=Set.empty
    , round=0
    }


start : Cmd Msg
start =
    Task.perform
        (always Start)
        -- (Task.succeed ())
        (Process.sleep 0)

update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Start ->
            let
                (newMap, moveCount) = applyRound state.map state.round
                output =
                    if modBy state.interval state.round == 0
                    then
                        interpolate "{1}\n\n at {0}\n"
                            [ S.fromInt (state.round + 1)
                            , renderMap newMap
                            ]
                        |> Just
                    else
                        Nothing
                again = state.map /= newMap
            in
            ( { state | map = newMap, round = state.round + 1 }
            , if again then
                Cmd.batch
                    [ start
                    , case output of
                        Just s -> consoleLog s []
                        Nothing -> Cmd.none
                    ]
              else
                consoleLog ("--- done at " ++ S.fromInt (state.round + 1) ++ "\n") []
            )


main : Program (List String) State Msg
main =
    worker
        { init =
            \argv ->
                let
                    initialState =
                        { interval = case argv|>L.filterMap S.toInt of
                            [n] -> n
                            _ -> 100
                        , round = 0
                        , map = parse input
                        }
                in
                ( initialState
                , start
                )
        , subscriptions = \n -> Sub.none
        , update = update
        }
