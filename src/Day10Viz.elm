module Day10Viz exposing (..)

import Color as ElmColor
import Day10
import Day10Input
import List
import List.Extra as LE
import Maybe
import Playground exposing (..)


type alias State =
    { xvals : List Int, pixels : List Bool, time : Float }


initialState : State
initialState =
    { xvals = Day10.xvalues Day10Input.input
    , pixels = Day10.pixels Day10Input.input
    , time = 0
    }


main =
    game view update initialState


backgroundColor =
    rgb 50 100 150


cellSize =
    10


view : Computer -> State -> List Shape
view computer state =
    let
        time = state.time -- floor state.time |> toFloat
        back =
            group
                [ rectangle backgroundColor computer.screen.width computer.screen.height
                , rectangle (rgb 255 50 50) computer.screen.width 1
                ]

        cycles =
            state.xvals
                --|> List.drop (time |> round)
                |> List.indexedMap
                    (\i x ->
                        rectangle (rgb 50 150 50) (3*cellSize) cellSize
                            |> move (toFloat (x - 20) * cellSize) (-cellSize / 2 - toFloat i * cellSize)
                    )
                |> group
                |> moveY (time * cellSize)

        display =
            [ rectangle (rgb 200 200 200) (cellSize * 40) (cellSize * 6)
                |> move (-cellSize / 2) (-cellSize * 6 / 2 + 0)
                |> fade 0.5
             ]
                ++ (state.pixels
                        |> List.take ((time + 1 )|> ceiling)
                        |> LE.init
                        |> Maybe.withDefault []
                        |> List.indexedMap
                            (\i b ->
                                if b then
                                    Just
                                        (rectangle
                                            (if b then
                                                rgb 0 0 0

                                             else
                                                rgb 255 255 255
                                            )
                                            cellSize
                                            cellSize
                                            |> move (toFloat (modBy 40 i - 20) * cellSize) (-cellSize / 2 - toFloat (i // 40) * cellSize)
                                        )

                                else
                                    Nothing
                            )
                        |> List.filterMap identity
                   )
                |> group
                |> move 0 (((time |> floor) // 40) * cellSize |> toFloat)

        cursor = rectangle (rgb 255 50 50) (cellSize*1.2) (cellSize*1.2)
                    |> move
                        (toFloat (modBy 40 (round time) - 20) * cellSize)
                        (-cellSize / 2)
                    |> fade 0.5
    in
    [ back, cycles, display, cursor ]


update : Computer -> State -> State
update computer state =
    let
        newtime =
            state.time +
                if state.time < 10 then 0.05
                else if state.time < 40 then 0.1
                else 0.5
    in
    { state
        | time =
            if newtime > 6 * 40 then
                0

            else
                newtime
    }
