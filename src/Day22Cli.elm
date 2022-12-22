port module Day22Cli exposing (..)

import Day22 exposing (..)
import Day22Input exposing (..)
import Json.Encode as Enc
import List as L
import Platform exposing (worker)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Process
import Stat
import String as S
import String.Interpolate exposing (interpolate)
import Task
import Time
import Debug as D
import Dict exposing (Dict)
import Array exposing (Array)
import AocUtil exposing (fromJust)
import List.Extra as LE

port jsonConsole : Enc.Value -> Cmd msg


consoleLog str args =
    interpolate str args |> Enc.string |> jsonConsole


type alias State =
    { bpidx: Int }


type Msg
    = Start


emptyState : State
emptyState =
    { bpidx = 0 }


start =
    Task.perform
        (\_ -> Start)
        (Process.sleep 10)

type alias Step =
    { time: Int
    , coords: Coords
    , facing0: Facing
    , facing1: Facing
    , folding: Bool
    }


makeOutput =
    let
        (tiles, instructions) = parse input

        faceIndex (x, y) =
            3 * ((y-1) // 50)
            + ((x-1) // 50)

        path : List Step
        path =
            instructions
            |> makePath tiles
            |> LE.group |> L.map (\(x,_) -> x)
            |> LE.groupWhile (\(pos1, _) (pos2, _) -> pos1 == pos2)
            |> L.indexedMap (\i ((pos, f0), sts) ->
                let
                    f1 = sts |> L.map Tuple.second |> LE.last |> Maybe.withDefault f0
                in
                { time=i
                , coords=pos
                , facing0=f0
                , facing1=f1
                , folding=False
                }
            )
            |> LE.greedyGroupsOfWithStep 2 1
            |> L.map (\pair ->
                case pair of
                    [ st1, st2 ] -> { st1 | folding = faceIndex st1.coords /= faceIndex st2.coords }
                    [ st1 ] -> st1
                    _ -> D.todo "oops"
                )


        faces =
            L.range 0 2
            |> L.concatMap (\fx ->
                L.range 0 3
                |> L.map (\fy ->
                    (fx, fy, Dict.values tiles
                    |> L.filter (\t ->
                        let
                            tfx = ((t.coords|>Tuple.first) - 1) // 50
                            tfy = ((t.coords|>Tuple.second) - 1) // 50
                        in
                        tfx == fx && tfy == fy
                    ))
                )
            )

        pathFaces: List (Int, Int, List Step)
        pathFaces =
            L.range 0 2
            |> L.concatMap (\fx ->
                L.range 0 3
                |> L.map (\fy ->
                    (fx, fy,
                        path
                        |> L.filterMap (\step ->
                            let
                                (x, y) = step.coords
                                tfx = (x - 1) // 50
                                tfy = (y - 1) // 50
                            in
                            if tfx == fx && tfy == fy then
                                Just { step | coords = (modBy 50 (x-1), modBy 50 (y-1)) }
                            else
                                Nothing
                        ))
                )
            )

        facesScad =
            L.map2 (\(fx, fy, ts) (_, _, steps) ->
                interpolate """module face_{0}_{1}() { {2} }"""
                [ fx|>S.fromInt
                , fy|>S.fromInt
                , (ts
                    |> L.filter (\t -> t.isWall)
                    |> L.map (\t ->
                        interpolate "translate([{0}, {1}, 0]) wall();"
                          [ ((t.coords|>Tuple.first) - 1 - 50*fx)|>S.fromInt
                          , (49 - ((t.coords|>Tuple.second) - 1 - 50*fy))|>S.fromInt ]
                        )
                  ) ++ (steps
                    |> L.map (\step ->
                        let
                            (x, y) = step.coords
                        in
                        interpolate """translate([{0}, {1}, 0]) step({2}, {3}, {4}, {5});"""
                        [ S.fromInt x, S.fromInt (49-y), S.fromInt step.facing0, S.fromInt step.facing1, if step.folding then "1" else "0", S.fromInt step.time ])
                  )
                    |> S.join "\n" ]
                ) faces pathFaces
            |> S.join "\n\n"
        output = interpolate """
        {0}

        $t={1};
        $window={1};

        module step(facing0, facing1, folding, timestamp) {
            if (timestamp <= $t && timestamp >= ($t-$window)) {
                h = .5-.5*($t-timestamp)/$window;

                color([0,($t-timestamp)/$window,1])

                translate([0,0,h])

                translate([0.5, 0.5, 0])
                {
                    rotate([0, 0, -90*facing0])
                    {
                        translate([-0.2, 0, 0])
                        cube([.6, .2, 0.01], center=true);
                    }
                    rotate([0, 0, -90*facing1])
                    {
                        translate([0.2, 0, 0])
                        cube([.6, .2, 0.01], center=true);

                        if(folding) {
                            translate([.5,0,0])
                            rotate([0, 45, 0])
                            translate([0, -0.1, 0])
                            cube([sqrt(2)*h ,.2,0.01],);
                        }
                    }


                    if(timestamp == $t)
                         sphere(2);
                    if(timestamp == $t-$window)
                         cube(2, center=true);
                }

            }
        }

        module wall() {
            cube(1);
        }

        color("green")
        cube(50);

        rotate([90, 0, 0])
        face_1_0();

        translate([50, 0, 0])
        rotate([90, 0, 90])
        face_2_0();

        translate([0, 50, 0])
        rotate([180, 0, 0])
        face_1_1();

        translate([0, 50, 50])
        rotate([-90, 0, 0])
        face_1_2();

        translate([0, 0, 50])
        rotate([-90, 0, 90])
        face_0_2();

        translate([50, 0, 50])
        rotate([0, 0, 90])
        face_0_3();

        translate([-20, -480, 60])
        scale(10)
        face_0_0();

        """
            [ facesScad, (L.length path - 1) |> S.fromInt ]
    in
    output

update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Start ->
            let
                output = makeOutput
            in
            ( state
            , consoleLog output []
            )


main : Program (List String) State Msg
main =
    worker
        { init =
            \argv ->
                let
                    _ = argv
                in
                ( emptyState
                , start
                )
        , subscriptions = \n -> Sub.none
        , update = update
        }
