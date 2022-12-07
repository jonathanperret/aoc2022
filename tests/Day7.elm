module Day7 exposing (..)

import AocUtil exposing (..)
import Day7Input exposing (input)
import Expect
import Fuzz
import List.Extra
import Set
import String.Extra
import Test exposing (..)
import Tuple


example1 =
    """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""


suite : Test
suite =
    describe "day 7"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> Expect.equal 95437
            , test "input" <| \_ -> input |> part1 |> Expect.equal 1084134
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> Expect.equal 24933642
            , test "input" <| \_ -> input |> part2 |> Expect.equal 6183184
            ]
        ]


dirSizes pathSizes =
    let
        expandedPathSizes : List ( String, Int )
        expandedPathSizes =
            pathSizes
                |> List.concatMap
                    (\( name, size ) ->
                        let
                            components =
                                name |> String.split "/" |> List.Extra.init |> fromJust

                            prefixes =
                                components |> List.Extra.inits |> List.drop 1
                        in
                        prefixes |> List.map (\p -> ( p |> String.join "/", size ))
                    )

    in
    expandedPathSizes
        |> List.Extra.gatherEqualsBy Tuple.first
        |> List.map
            (\( ( name, size ), others ) ->
                ( name, size + (List.map Tuple.second others |> List.sum) )
            )


sumSmallDirectories pathSizes =
    pathSizes
        |> dirSizes
        |> List.map Tuple.second
        |> List.filter (\s -> s <= 100000)
        |> List.sum


executeCommand cmd ( pathSizes, current ) =
    case String.slice 0 4 cmd of
        "$ cd" ->
            let
                newdir =
                    String.dropLeft 5 cmd
            in
            case newdir of
                ".." ->
                    ( pathSizes, String.Extra.leftOfBack "/" current )

                "/" ->
                    ( pathSizes, "" )

                _ ->
                    ( pathSizes, [ current, "/", newdir ] |> String.concat )

        "$ ls" ->
            ( pathSizes, current )

        "dir " ->
            ( pathSizes, current )

        _ ->
            let
                name =
                    cmd |> String.Extra.rightOf " "

                size =
                    cmd |> String.Extra.leftOf " " |> String.toInt |> fromJust
            in
            ( ( [ current, "/", name ] |> String.concat, size ) :: pathSizes, current )


buildPathSizes input =
    input
        |> String.lines
        |> List.foldl executeCommand ( [], "" )
        |> Tuple.first


part1 input =
    buildPathSizes input
        |> sumSmallDirectories


part2 input =
    let
        pathSizes =
            buildPathSizes input

        sizes =
            dirSizes pathSizes

        totalSize =
            sizes
                |> List.Extra.minimumBy Tuple.first
                |> fromJust
                |> Tuple.second
    in
    sizes
        |> List.map Tuple.second
        |> List.filter (\s -> totalSize - s <= 40000000)
        |> List.minimum
        |> fromJust
