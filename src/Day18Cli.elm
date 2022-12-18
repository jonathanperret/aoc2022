port module Day18Cli exposing (..)

import Day18 exposing (..)
import Day18Input exposing (..)
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


port jsonConsole : Enc.Value -> Cmd msg


consoleLog str args =
    interpolate str args |> Enc.string |> jsonConsole


type alias State =
    { repeats : Int, durations : List Int }


type Msg
    = Start
    | Print { result : Int, duration : Int }


emptyState : State
emptyState =
    { repeats = 0, durations = [] }


start =
    Task.perform
        (\_ -> Start)
        (Process.sleep 10)


measure : (() -> a) -> Task.Task x ( Int, a )
measure work =
    Time.now
        |> Task.andThen
            (\startT ->
                work ()
                    |> (\result ->
                            Time.now
                                |> Task.andThen
                                    (\finishT ->
                                        Task.succeed ( Time.posixToMillis finishT - Time.posixToMillis startT, result )
                                    )
                       )
            )


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Start ->
            ( state
            , measure (\_ -> part2 input)
                |> Task.perform (\( duration, result ) -> Print { result = result, duration = duration })
            )

        Print { result, duration } ->
            ( { state | repeats = state.repeats - 1, durations = duration :: state.durations }
            , Cmd.batch
                [ consoleLog "repeat {0}: duration={1}" [ state.repeats |> S.fromInt, duration |> S.fromInt ]
                , if state.repeats == 0 then
                    let
                        average =
                            state.durations |> L.map toFloat |> Stat.mean |> Maybe.withDefault 0
                    in
                    consoleLog "average={0}" [ average |> S.fromFloat ]

                  else
                    start
                ]
            )


main : Program (List String) State Msg
main =
    worker
        { init =
            \argv ->
                let
                    repeats =
                        L.head argv |> Maybe.andThen String.toInt |> Maybe.withDefault 1
                in
                ( { emptyState | repeats = repeats }
                , Cmd.batch [ consoleLog "repeats: {0}" [ repeats |> S.fromInt ], start ]
                )
        , subscriptions = \n -> Sub.none
        , update = update
        }
