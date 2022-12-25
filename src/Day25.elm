module Day25 exposing (..)

import AocUtil exposing (fromJust)
import Array exposing (Array)
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
                    (\r val -> r + val * power
                     --|> D.log (interpolate "{0}*{1}+{2}" [ S.fromInt r, S.fromInt val, S.fromInt power ])
                    )
                    result
                    (fromSnafuDigit digit)
                , power * 5
                )
            )
            ( Just 0, 1 )
        |> (\( result, _ ) -> result)


toSnafu n =
    let
        snafuZero =
            fromSnafu "====================" |> fromJust

        nFromZero =
            n - snafuZero

        powers =
            L.range 0 20
                |> L.foldl (\_ ( ps, p ) -> ( p :: ps, p * 5 )) ( [], 1 )
                |> Tuple.first

        takeFrom rest p res =
            if rest >= p then
                takeFrom (rest - p) p (res + 1)

            else
                ( rest, res )

        ( _, resultDigits ) =
            powers
                |> L.foldl
                    (\p ( rest, digits ) ->
                        let
                            ( rest2, digit ) =
                                takeFrom rest p 0
                        in
                        ( rest2, digit :: digits )
                    )
                    ( nFromZero, [] )
    in
    resultDigits
        |> L.reverse
        |> L.drop 1
        |> LE.dropWhile (\d -> d == 2)
        |> L.map
            (\d ->
                case d of
                    0 ->
                        '='

                    1 ->
                        '-'

                    2 ->
                        '0'

                    3 ->
                        '1'

                    4 ->
                        '2'

                    _ ->
                        '!'
            )
        |> S.fromList


part1 input =
    let
        result =
            input
                |> S.lines
                |> L.map (fromSnafu >> fromJust)
                |> L.sum
    in
    ( result, toSnafu result )


part2 : String -> Int
part2 input =
    0
