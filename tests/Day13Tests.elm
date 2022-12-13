module Day13Tests exposing (..)

import AocUtil exposing (..)
import Array exposing (Array)
import Basics.Extra exposing (..)
import Day13 exposing (..)
import Day13Input exposing (input)
import Debug as D
import Expect
import Fuzz
import Json.Decode as JD
import List as L
import List.Extra as LE
import Set
import String as S
import String.Extra
import Test exposing (..)
import Tuple


example1 =
    [ Msg [ Num 1, Num 1, Num 3, Num 1, Num 1 ]
    , Msg [ Num 1, Num 1, Num 5, Num 1, Num 1 ]
    , Msg [ Msg [ Num 1 ], Msg [ Num 2, Num 3, Num 4 ] ]
    , Msg [ Msg [ Num 1 ], Num 4 ]
    , Msg [ Num 9 ]
    , Msg [ Msg [ Num 8, Num 7, Num 6 ] ]
    , Msg [ Msg [ Num 4, Num 4 ], Num 4, Num 4 ]
    , Msg [ Msg [ Num 4, Num 4 ], Num 4, Num 4, Num 4 ]
    , Msg [ Num 7, Num 7, Num 7, Num 7 ]
    , Msg [ Num 7, Num 7, Num 7 ]
    , Msg []
    , Msg [ Num 3 ]
    , Msg [ Msg [ Msg [] ] ]
    , Msg [ Msg [] ]
    , Msg [ Num 1, Msg [ Num 2, Msg [ Num 3, Msg [ Num 4, Msg [ Num 5, Num 6, Num 7 ] ] ] ], Num 8, Num 9 ]
    , Msg [ Num 1, Msg [ Num 2, Msg [ Num 3, Msg [ Num 4, Msg [ Num 5, Num 6, Num 0 ] ] ] ], Num 8, Num 9 ]
    ]


suite : Test
suite =
    describe "day 13"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> Expect.equal 13
            , test "input" <| \_ -> input |> part1 |> Expect.equal 5292
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 140
            , test "input" <| \_ -> input |> part2 |> Expect.equal 23868
            ]
        ]


toList : Message -> List (List (List (List (List (List Int)))))
toList =
    let
        fromNum m =
            case m of
                Num x -> x
                Msg _ ->
                    Debug.todo "too deep"

        asList m =
            case m of
                Num x -> [ m ]
                Msg xs -> xs
    in
    asList >> L.map (
        asList >> L.map (
            asList >> L.map (
                asList >> L.map (
                    asList >> L.map (
                        asList >> L.map fromNum)))))


part1 input =
    input
        |> L.map toList
        |> LE.groupsOf 2
        |> LE.indexedFoldl
            (\i pair acc ->
                case pair of
                    [ m1, m2 ] ->
                        if m1 < m2 then
                            acc + i + 1

                        else
                            acc

                    _ ->
                        Debug.todo "bad pair"
            )
            0


markers =
    [ Msg [ Msg [ Num 2 ] ], Msg [ Msg [ Num 6 ] ] ]
    |> L.map toList


part2 input =
    input
        |> L.map toList
        |> L.append markers
        |> L.sort
        |> LE.findIndices (\m -> L.member m markers)
        |> L.foldl (\i r -> (i + 1) * r) 1
