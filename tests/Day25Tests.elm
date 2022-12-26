module Day25Tests exposing (..)

import AocUtil exposing (..)
import Array exposing (Array)
import Day25 exposing (..)
import Day25Input exposing (..)
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
import Test exposing (..)
import Tuple


example1 =
    """1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122"""


example1output =
    [ 1747
    , 906
    , 198
    , 11
    , 201
    , 31
    , 1257
    , 32
    , 353
    , 107
    , 7
    , 3
    , 37
    ]


example2 =
    [ ( 0, "0" )
    , ( 1, "1" )
    , ( 2, "2" )
    , ( 3, "1=" )
    , ( 4, "1-" )
    , ( 5, "10" )
    , ( 6, "11" )
    , ( 7, "12" )
    , ( 8, "2=" )
    , ( 9, "2-" )
    , ( 10, "20" )
    , ( 15, "1=0" )
    , ( 20, "1-0" )
    , ( 2022, "1=11-2" )
    , ( 12345, "1-0---0" )
    , ( 314159265, "1121-1110-1=0" )
    , ( 100314159265, "1=121-21-1110-1=0" )
    , ( -100314159265, "-2-=-1=-1---01-20" )
    ]


suite : Test
suite =
    describe "day 25"
        [ describe "part 1"
            [ test "fromSnafu" <| \_ -> example1 |> S.lines |> L.filterMap fromSnafu |> Expect.equal example1output
            , test "toSnafu" <|
                \_ ->
                    example2
                        |> L.map
                            (Tuple.mapFirst toSnafu
                                >> Tuple.mapSecond (fromSnafu >> fromJust)
                                >> (\( a, b ) -> ( b, a ))
                            )
                        |> Expect.equal example2
            , test "example" <| \_ -> example1 |> part1 |> Expect.equal ( 4890, "2=-1=0" )
            , test "fromSnafuBig" <| \_ -> "122-2=200-0111--=200" |> fromSnafu |> Expect.equal (Just 28127432121050)
            , test "input" <| \_ -> input |> part1 |> Expect.equal ( 28127432121050, "122-2=200-0111--=200" )
            , test "largestSafeSnafu" <| \_ -> largestSafeSnafu (2 ^ 53) |> Expect.equal 5960464477539062
            ]
        ]
