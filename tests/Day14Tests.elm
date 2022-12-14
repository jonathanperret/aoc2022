module Day14Tests exposing (..)

import AocUtil exposing (..)
import Array exposing (Array)
import Day14Input exposing (input)
import Debug as D
import Expect
import Fuzz
import List as L
import List.Extra as LE
import Regex
import Set
import String as S
import String.Extra
import Test exposing (..)
import Tuple


example1 =
    """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""


suite : Test
suite =
    describe "day 14"
        [ describe "part 1"
            [ test "parse" <|
                \_ ->
                    "498,4 -> 498,6 -> 496,6"
                        |> parseLine
                        |> Expect.equal
                            [ ( 498, 4 ), ( 498, 6 ), ( 496, 6 ) ]
            , test "expand" <|
                \_ ->
                    [ ( 498, 4 ), ( 498, 6 ), ( 496, 6 ) ]
                        |> expand
                        |> Expect.equal
                            (Set.fromList [ ( 498, 4 ), ( 498, 5 ), ( 498, 6 ), ( 497, 6 ), ( 496, 6 ) ])
            , test "dropOne" <|
                \_ ->
                    Set.fromList [ ( 501, 4 ), ( 501, 5 ), ( 501, 6 ), ( 500, 6 ), ( 499, 6 ) ]
                        |> dropOne
                        |> Expect.equal
                            (Set.fromList [ ( 501, 4 ), ( 501, 5 ), ( 501, 6 ), ( 500, 6 ), ( 499, 6 ), ( 500, 5 ) ])
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal 24
            , test "input" <| \_ -> input |> part1 |> Expect.equal 618
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 93
            , test "input" <| \_ -> input |> part2 |> Expect.equal 26358
            ]
        ]


parseLine line =
    Regex.find (Regex.fromString "\\d+" |> Maybe.withDefault Regex.never) line
        |> L.map .match
        |> L.map (String.toInt >> fromJust)
        |> LE.groupsOf 2
        |> L.map
            (\g ->
                case g of
                    [ a, b ] ->
                        ( a, b )

                    _ ->
                        Debug.todo "bad pair"
            )


expand points =
    points
        |> LE.groupsOfWithStep 2 1
        |> L.map
            (\pair ->
                case pair of
                    [ p1, ( x2, y2 ) ] ->
                        let
                            step ( x, y ) =
                                let
                                    ( nx, ny ) =
                                        ( if x < x2 then
                                            x + 1

                                          else if x > x2 then
                                            x - 1

                                          else
                                            x
                                        , if y < y2 then
                                            y + 1

                                          else if y > y2 then
                                            y - 1

                                          else
                                            y
                                        )
                                in
                                ( nx, ny )
                                    :: (if ( nx, ny ) == ( x2, y2 ) then
                                            []

                                        else
                                            step ( nx, ny )
                                       )
                        in
                        step p1

                    _ ->
                        Debug.todo "bad points"
            )
        |> L.concat
        |> Set.fromList
        |> Set.insert (L.head points |> fromJust)


dropOne points =
    dropFrom ( 500, 0 ) points


dropFrom : ( Int, Int ) -> Set.Set ( Int, Int ) -> Set.Set ( Int, Int )
dropFrom ( x, y ) points =
    let
        spot =
            [ ( x, y + 1 ), ( x - 1, y + 1 ), ( x + 1, y + 1 ) ]
                |> LE.find (\p -> Set.member p points |> not)
    in
    case spot of
        Just p ->
            if y < 1000 then
                dropFrom p points

            else
                points

        Nothing ->
            Set.insert ( x, y ) points


part1 input =
    let
        points0 =
            input
                |> S.lines
                |> L.map (parseLine >> expand)
                |> L.foldl Set.union Set.empty

        loop points n =
            let
                newPoints =
                    dropOne points
            in
            if Set.size newPoints == Set.size points then
                n

            else
                loop newPoints (n + 1)
    in
    loop points0 0


addFloor : List (List ( Int, Int )) -> List (List ( Int, Int ))
addFloor lines =
    let
        ymax =
            lines
                |> L.concat
                |> L.map Tuple.second
                |> L.maximum
                |> fromJust
    in
    [ ( 500 - (ymax + 2), ymax + 2 ), ( 500 + (ymax + 2), ymax + 2 ) ] :: lines


part2 input =
    let
        points0 =
            input
                |> S.lines
                |> L.map parseLine
                |> addFloor
                |> L.map expand
                |> L.foldl Set.union Set.empty

        loop points n =
            let
                newPoints =
                    dropOne points
            in
            if Set.size newPoints == Set.size points then
                n

            else
                loop newPoints (n + 1)
    in
    loop points0 0
