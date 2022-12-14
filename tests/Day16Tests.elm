module Day16Tests exposing (..)

import AocUtil exposing (..)
import Day16Input exposing (..)
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
import Debug as D exposing (log, todo)
import Array exposing (Array)
import Regex
import Dict exposing (Dict)
import Day16 exposing (..)

suite : Test
suite =
    describe "day 16"
        [ describe "part 2"
            [ test "parseLine" <|\_-> "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
                |> parseLine |> Expect.equal { name="AA", rate=0, next=["DD","II","BB"] }
            , test "example" <| \_ -> example1 |> part2 |> Expect.equal 1707
            , skip <| test "input" <| \_ -> input |> part2 |> Expect.equal 0
            -- (2496,"23:0->MB ; 21:1->IV ; 20:0->AQ ; 18:1->QR ; 14:0->TL ; 12:1->QB ; 11:0->LG ; 8:1->XK ; 7:0->WV ; 5:1->DC ; 3:0->OF ; 1:1->SB")
            ]
        ]


part2 input =
    let
        dict =
            input
            |> parse
            --|> Debug.log "initial"

        cm = costMatrix dict

        _ = dict
            --|> Debug.log ("dict " ++ (Debug.toString (Dict.size dict)))
        dict2 =
            dict
            |> Dict.filter (\k v -> v.rate > 0)

        best = bestPath dict2 cm 26 { people = [("AA",0),("AA",0)] } 0 ""

        _ = (best.value,
            L.map (\step->(Debug.toString step.t++":"++(D.toString step.who)++"->"++step.to)) best.steps
            |> String.join " ; ")
            |> Debug.log "best"

        --ps2 = ps |> Set.fromList |> Set.toList |> L.length |> Debug.log "ps2"
    in
    best.value

{-
20*(30-2)+13*(30-5)+11*(30-9)+22*(30-17)+3*(30-21)+2*(30-24)
-}
