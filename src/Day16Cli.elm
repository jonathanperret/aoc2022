port module Day16Cli exposing (..)

import Json.Encode as E exposing (Value, int, object, string)
import Platform exposing (worker)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import List as L
import AocUtil exposing (..)
import Day16Input exposing (input)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set exposing (Set)
import String.Extra
import Tuple
import String as S
import Debug as D exposing (log, todo)
import Array exposing (Array)
import Regex
import Dict exposing (Dict)

import Day16 exposing (..)
import Day16Input exposing (..)


port jsonConsole : Value -> Cmd msg

part2 input =
    let
        dict =
            input
            |> parse
            --|> Debug.log "initial"

        cm = costMatrix dict

        _ = dict
            --|> Debug.log ("dict " ++ (Debug.toString (Dict.size dict)))
        dict2 =
            dict
            |> Dict.filter (\k v -> v.rate > 0)

        best = bestPath dict2 cm 26 { people = [("AA",0),("AA",0)] } 0 ""

        _ = (best.value,
            L.map (\step->(Debug.toString step.t++":"++(D.toString step.who)++"->"++step.to)) best.steps
            |> String.join " ; ")
            |> Debug.log "best"

        --ps2 = ps |> Set.fromList |> Set.toList |> L.length |> Debug.log "ps2"
    in
    best.value

main : Program () number String
main =
    worker
        { init =
            \_ ->
                let
                    _ = part2 input
                        |> Debug.log "found"
                in
                ( 1, [E.string "ahaha"] |> L.map jsonConsole |> Cmd.batch)
        , subscriptions = \n -> Sub.none
        , update = \m n -> ( n, jsonConsole (E.string "update") )
        }
