port module Main exposing (..)

import Json.Encode as E exposing (Value, int, object, string)
import Platform exposing (worker)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import List as L


port jsonConsole : Value -> Cmd msg


main : Program () number String
main =
    worker
        { init =
            \_ ->
                ( 1, [E.int 123, E.string "ahaha"] |> L.map jsonConsole |> Cmd.batch)
        , subscriptions = \n -> Sub.none
        , update = \m n -> ( n, jsonConsole (E.string "update") )
        }
