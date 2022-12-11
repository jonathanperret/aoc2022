module Day10 exposing (..)

import List as L
import List.Extra as LE


xvalues : String -> List Int
xvalues input =
    let
        ( finalX, finalPastXs ) =
            String.lines input
                |> L.foldl
                    (\instr ( x, pastXs ) ->
                        case
                            instr
                                |> String.words
                                |> List.tail
                                |> Maybe.andThen List.head
                                |> Maybe.andThen String.toInt
                        of
                            Just val ->
                                ( x + val, x :: x :: pastXs )

                            _ ->
                                ( x, x :: pastXs )
                    )
                    ( 1, [] )
    in
    (finalX :: finalPastXs) |> L.reverse


part1 input =
    xvalues input
        |> L.indexedMap
            (\c x ->
                if (c + 1 - 20 |> modBy 40) == 0 then
                    Just ( c + 1, x )

                else
                    Nothing
            )
        |> L.filterMap identity
        |> L.map (\( c, x ) -> c * x)
        |> L.sum

pixels : String -> List Bool
pixels input =
    xvalues input
        |> L.indexedMap
            (\c x ->
                let
                    col =
                        modBy 40 c

                in x == col || (x - 1) == col || (x + 1) == col
            )

part2 : String -> List String
part2 input =
    pixels input
        |> L.map (\hit ->
                if hit then
                    "#"

                else
                    ".")
        |> LE.groupsOf 40
        |> L.map (String.join "")
