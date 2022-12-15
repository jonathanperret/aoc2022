module Day15Tests exposing (..)

import AocUtil exposing (..)
import Day15Input exposing (input)
import Expect
import Fuzz
import List.Extra
import List.Extra as LE
import Set exposing (Set)
import String.Extra
import Test exposing (..)
import Tuple
import String as S
import List as L
import Debug as D
import Array exposing (Array)
import Regex


example1 = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"""


suite : Test
suite =
    describe "day 15"
        [ describe "part 1"
            [ test "top" <|\_-> seenOnRow -2 { bx = 2, by = 10, sx = 8, sy = 7 } |> Expect.equal (Just (8, 8))
            , test "row2" <|\_-> seenOnRow -1 { bx = 2, by = 10, sx = 8, sy = 7 } |> Expect.equal (Just (7, 9))
            , test "middle" <|\_-> seenOnRow 7 { bx = 2, by = 10, sx = 8, sy = 7 } |> Expect.equal (Just (-1, 17))
            , test "bottom" <|\_-> seenOnRow 16 { bx = 2, by = 10, sx = 8, sy = 7 } |> Expect.equal (Just (8, 8))
            , test "beacon" <|\_-> seenOnRow 10 { bx = 2, by = 10, sx = 8, sy = 7 } |> Expect.equal (Just (3, 14))
            , test "beacon topright" <|\_-> seenOnRow 10 { bx = 2, by = 10, sx = 0, sy = 11 } |> Expect.equal (Just (-2, 1))
            , test "beacon bottom" <|\_-> seenOnRow 10 { bx = 2, by = 10, sx = 2, sy = 0 } |> Expect.equal Nothing
            , test "example" <| \_ -> example1 |> part1 10 |> Expect.equal 26
            , test "input" <| \_ -> input |> part1 2000000 |> Expect.equal 4724228
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 20 |> Expect.equal 56000011
            , test "input" <| \_ -> input |> part2 4000000 |> Expect.equal 13622251246513
            ]
        ]

type alias Sensor = { sx: Int, sy: Int, bx: Int, by: Int }

parseLine: String -> Sensor
parseLine line = 
    let
        re = Regex.fromString "-?\\d+"|>Maybe.withDefault Regex.never
    in
    case Regex.find re line |> List.map .match |> List.map String.toInt of
        [Just sx, Just sy, Just bx, Just by] ->
            { sx=sx, sy=sy, bx=bx, by=by }
        _ -> Debug.todo ("bad line " ++ line)

seenOnRow : Int -> Sensor -> Maybe (Int, Int)
seenOnRow y0 { sx, sy, bx, by } =
    let
        _ = if sx == bx && sy == by then Debug.todo "oops" else ()

        dist = abs ( bx - sx ) + abs ( by - sy )
        xmin = sx - dist
        xmax = sx + dist
        ymin = sy - dist
        ymax = sy + dist
        dxmin = if by == y0 && bx <=sx then 1 else 0
        dxmax = if by == y0 && bx >=sx then 1 else 0
        rmin = xmin + abs (sy - y0) + dxmin
        rmax = xmax - abs (sy - y0) - dxmax
    in
    if rmin > rmax then
        Nothing
    else
        Just (rmin, rmax)

seenOnRowNoBeacon : Int -> Sensor -> Maybe (Int, Int)
seenOnRowNoBeacon y0 { sx, sy, bx, by } =
    let
        _ = if sx == bx && sy == by then Debug.todo "oops" else ()

        dist = abs ( bx - sx ) + abs ( by - sy )
        xmin = sx - dist
        xmax = sx + dist
        ymin = sy - dist
        ymax = sy + dist
        rmin = xmin + abs (sy - y0)
        rmax = xmax - abs (sy - y0)
    in
    if rmin > rmax then
        Nothing
    else
        Just (rmin, rmax)

mergeSpans: (Int, Int) -> (Int, Int) -> List (Int, Int)
mergeSpans (xmin1, xmax1) (xmin2, xmax2) =
    (if xmin1 > xmin2 then mergeSpans (xmin2, xmax2) (xmin1, xmax1)
    else if xmax1 < (xmin2 - 1) then [(xmin2, xmax2), (xmin1, xmax1)]
    else if xmax2 < xmax1 then [ (xmin1, xmax1) ]
    else [ (xmin1, xmax2) ])
    --|>Debug.log ("merging " ++ Debug.toString (xmin1,xmax1) ++ Debug.toString (xmin2, xmax2))

part1 targetRow input =
    let
        sensors =
            input
            |> S.lines
            |> L.map parseLine

        --_ = (\s -> (s, seenOnRow -2 s) |> Debug.log "seen") ({ bx = 2, by = 10, sx = 8, sy = 7 })
        --_ = List.map (\s -> (s, seenOnRow -2 s) |> Debug.log "seen") sensors

        spans = L.filterMap (\s -> seenOnRow targetRow s |> Debug.log ("seen " ++ Debug.toString s)) sensors

        merged =
            spans
            |> L.sortBy Tuple.first
            |> Debug.log "sorted"
            |> L.foldl (\s ss -> (case ss of
                [] -> [s]
                h::t -> mergeSpans h s ++ t) |> Debug.log ("merge " ++ Debug.toString s ++ Debug.toString ss)) []
            |> Debug.log "merged"
            |> L.map (\(xmin, xmax) -> xmax - xmin + 1)
            |> Debug.log "lengths"
            |> L.foldl (+) 0
    in
    merged

intersectSpans: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
intersectSpans (xmin1, xmax1) (xmin2, xmax2) =
    (if xmin1 > xmin2 then intersectSpans (xmin2, xmax2) (xmin1, xmax1)
    else if xmax1 < xmin2 then Nothing
    else if xmax2 < xmax1 then Just (xmin2, xmax2)
    else Just (xmin2, xmax1))

part2 range input =
    let
        sensors =
            input
            |> S.lines
            |> L.map parseLine

        doRow y0 =
                let
                    spans = L.filterMap (\s -> seenOnRowNoBeacon y0 s) sensors

                    merged =
                        spans
                        |> L.sortBy Tuple.first
                        |> L.foldl (\s ss -> (case ss of
                            [] -> [s]
                            h::t -> mergeSpans h s ++ t)) []
                        |> L.sortBy Tuple.first
                        --|> Debug.log ("beforeint " ++ Debug.toString y0)
                        |> L.filterMap (intersectSpans (0, range))
                        --|> Debug.log ("spans " ++ Debug.toString y0)
                        |> L.map (\(xmin, xmax) -> xmax - xmin + 1)

                    found = L.length merged == 2

                in
                if found then
                    Just ((L.head merged |> fromJust) * 4000000 + y0)
                else
                    Nothing
    in
    L.range 0 range
    |> LE.findMap doRow
    |> fromJust
