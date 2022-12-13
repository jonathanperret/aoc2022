module Day13 exposing (..)

type Message =
    Num Int
    | Msg (List Message)


