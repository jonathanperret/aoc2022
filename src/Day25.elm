module Day25 exposing (..)

import AocUtil exposing (fromJust)
import Array exposing (Array)
import Debug as D
import Dict exposing (Dict)
import Expect
import Fuzz
import List as L
import List.Extra as LE exposing (unfoldr)
import Regex
import Set exposing (Set)
import String as S
import String.Extra
import String.Interpolate exposing (interpolate)
import Tuple


fromSnafuDigit digit =
    case digit of
        '=' ->
            Just -2

        '-' ->
            Just -1

        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        _ ->
            Nothing


fromSnafu s =
    s
        |> S.toList
        |> L.reverse
        |> L.foldl
            (\digit ( result, power ) ->
                ( Maybe.map2
                    (\r val ->
                        r
                            + val
                            * power
                     --|> D.log (interpolate "{0}*{1}+{2}" [ S.fromInt power, S.fromInt val, S.fromInt r ])
                    )
                    result
                    (fromSnafuDigit digit)
                , power * 5
                )
            )
            ( Just 0, 1 )
        |> (\( result, _ ) -> result)


divMod a b =
    floor (toFloat a / toFloat b) |> (\quot -> ( quot, a - b * quot ))


toSnafu : Int -> String
toSnafu =
    unfoldr
        (\n ->
            case n of
                0 ->
                    Nothing

                _ ->
                    let
                        ( quot, rem ) =
                            divMod (n + 2) 5
                    in
                    Just ( rem, quot )
        )
        >> LE.reverseMap (\d -> S.slice d (d + 1) "=-012")
        >> S.concat
        >> (\s ->
                if S.isEmpty s then
                    "0"

                else
                    s
           )


largestSafeSnafu : Int -> Int
largestSafeSnafu max =
    let
        check n =
            let
                roundTripped =
                    toSnafu n |> fromSnafu |> fromJust
            in
            n == roundTripped

        bisect lastGood firstBad =
            if firstBad - lastGood == 1 then
                lastGood

            else
                let
                    candidate =
                        lastGood
                            + floor ((toFloat firstBad - toFloat lastGood) / 2)
                in
                if check candidate then
                    bisect candidate firstBad

                else
                    bisect lastGood candidate
    in
    bisect 0 max


part1 input =
    let
        result =
            input
                |> S.lines
                |> L.map (fromSnafu >> fromJust)
                |> L.sum
    in
    ( result, toSnafu result )
