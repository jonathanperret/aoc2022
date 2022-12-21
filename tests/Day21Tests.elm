module Day21Tests exposing (..)

import AocUtil exposing (..)
import Array exposing (Array)
import Day21Input exposing (input)
import Debug as D
import Dict exposing (Dict)
import Expect
import Fuzz
import Integer exposing (Integer)
import List as L
import List.Extra as LE
import Maybe.Extra as Maybe
import Regex
import Set exposing (Set)
import String as S
import String.Extra
import String.Interpolate exposing (interpolate)
import Test exposing (..)
import Tuple
import BigRational exposing (BigRational)
import BigInt


example1 =
    """root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"""


suite : Test
suite =
    describe "day 21"
        [ describe "part 1"
            [ test "example" <| \_ -> example1 |> part1 |> ratStr |> Expect.equal "152"
            , test "input" <| \_ -> input |> part1 |> ratStr |> Expect.equal "82225382988628"
            ]
        , describe "part 2"
            [ test "example" <| \_ -> example1 |> part2 |> ratStr |> Expect.equal "301"
            , test "input" <| \_ -> input |> part2 |> ratStr |> Expect.equal "3429411069028"
            ]
        ]


type Op = Add | Sub | Mul | Div

parseOp opStr =
    case opStr of
        "+" -> Add

        "-" -> Sub

        "*" -> Mul

        "/" -> Div

        _ -> D.todo ("bad op " ++ opStr)

opToString op =
    case op of
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"

ratStr rat =
    let
        (num, denom) = rat |> BigRational.toBigInts
    in
        if denom == BigInt.fromInt 1 then
            BigInt.toString num
        else
        BigInt.toString num ++ "/" ++ BigInt.toString denom

type alias MonkeyRef = String

type Monkey
    = Literal BigRational
    | MonkeyOp Op MonkeyRef MonkeyRef

type Formula
    = FormLiteral BigRational
    | FormOp Op Formula Formula
    | FormVariable

parseLine : String -> (String, Monkey)
parseLine line =
    case
        Regex.find (Regex.fromString "[^ :]+" |> Maybe.withDefault Regex.never) line
            |> List.map .match
    of
        [ name, litStr ] ->
            (name, Literal (BigRational.fromString litStr |> fromJust))

        [ name, arg1, opStr, arg2 ] ->
            ( name
            , MonkeyOp (parseOp opStr) arg1 arg2
            )

        _ ->
            D.todo ("bad line " ++ line)


parse : String -> Dict String Monkey
parse input =
    input
        |> S.lines
        |> L.map parseLine
        |> Dict.fromList

evalOp: Op -> BigRational -> BigRational -> BigRational
evalOp op val1 val2 =
    case op of
        Add -> BigRational.add val1 val2
        Sub -> BigRational.sub val1 val2
        Mul -> BigRational.mul val1 val2
        Div ->
            let
                result = BigRational.div val1 val2
            in
            result

eval : Dict String Monkey -> String -> BigRational
eval monkeys name =
    let
        monkey =
            case Dict.get name monkeys of
                Just m ->
                    m

                _ ->
                    D.todo ("unknown monkey" ++ name)

        result = case monkey of
            Literal n ->
                n

            MonkeyOp op arg1 arg2 ->
                evalOp op (eval monkeys arg1) (eval monkeys arg2)

        -- _ = Integer.toString result |> D.log ("eval " ++ name)
    in
    if (result |> BigRational.toBigInts |> Tuple.second) /= BigInt.fromInt 1 then
        Debug.todo (interpolate "ooups evaluating {0} -> {1}"
            [ name, ratStr result ])
    else
        result


evalAllNonHuman : Dict String Monkey -> String -> Dict String BigRational -> Dict String BigRational
evalAllNonHuman monkeys name store =
    if name == "humn" then
        store

    else
        let
            monkey =
                case Dict.get name monkeys of
                    Just m ->
                        m

                    _ ->
                        D.todo ("unknown monkey" ++ name)

            (result, newStore) =
                case monkey of
                    Literal n ->
                        (Just n, store)

                    MonkeyOp op arg1 arg2 ->
                        let
                            st = store
                                    |> evalAllNonHuman monkeys arg1
                                    |> evalAllNonHuman monkeys arg2
                        in
                        (Maybe.map2 (evalOp op) (Dict.get arg1 st) (Dict.get arg2 st), st)
        in
        case result of
            Just r ->
                Dict.insert name r newStore

            Nothing ->
                newStore


solveTarget : Dict String Monkey -> Dict String BigRational -> String -> BigRational -> BigRational
solveTarget monkeys store name target =
    if name == "humn" then
        target

    else
        let
            monkey =
                case Dict.get name monkeys of
                    Just m ->
                        m

                    _ ->
                        D.todo ("unknown monkey" ++ name)

            result =
                case monkey of
                    Literal _ ->
                        D.todo "can't solve literal"

                    MonkeyOp op arg1 arg2 ->
                        let
                            vals = ( Dict.get arg1 store, Dict.get arg2 store )
                        in
                        case op of
                            Add ->
                                case vals of
                                    ( Nothing, Just val2 ) ->
                                        solveTarget monkeys store arg1 (BigRational.sub target val2)

                                    ( Just val1, Nothing ) ->
                                        solveTarget monkeys store arg2 (BigRational.sub target val1)

                                    _ ->
                                        D.todo "one of the branches needs to have a human"

                            Sub ->
                                case vals of
                                    ( Nothing, Just val2 ) ->
                                        solveTarget monkeys store arg1 (BigRational.add target val2)

                                    ( Just val1, Nothing ) ->
                                        solveTarget monkeys store arg2 (BigRational.sub val1 target)

                                    _ ->
                                        D.todo "one of the branches needs to have a human"

                            Mul ->
                                case vals of
                                    ( Nothing, Just val2 ) ->
                                        solveTarget monkeys store arg1 (BigRational.div target val2)

                                    ( Just val1, Nothing ) ->
                                        solveTarget monkeys store arg2 (BigRational.div target val1)

                                    _ ->
                                        D.todo "one of the branches needs to have a human"

                            Div ->
                                case vals of
                                    ( Nothing, Just val2 ) ->
                                        solveTarget monkeys store arg1 (BigRational.mul target val2)

                                    ( Just val1, Nothing ) ->
                                        solveTarget monkeys store arg2 (BigRational.div val1 target)

                                    _ ->
                                        D.todo "one of the branches needs to have a human"
        in
        result


solve : Dict String Monkey -> String -> BigRational
solve monkeys name =
    let
        monkey =
            case Dict.get name monkeys of
                Just m ->
                    m

                _ ->
                    D.todo ("unknown monkey" ++ name)

        store =
            evalAllNonHuman monkeys "root" Dict.empty

        result =
            case monkey of
                MonkeyOp Add arg1 arg2 ->
                    case ( Dict.get arg1 store, Dict.get arg2 store ) of
                        ( Nothing, Just val2 ) ->
                            solveTarget monkeys store arg1 val2

                        ( Just val1, Nothing ) ->
                            solveTarget monkeys store arg2 val1

                        _ ->
                            D.todo "one of the branches needs to have a human"

                _ ->
                    D.todo "can't solve this"
    in
    result


getFormula: Dict String Monkey -> MonkeyRef -> Formula
getFormula monkeys name =
    if name == "humn" then
        FormVariable
    else
    let
        monkey = Dict.get name monkeys |> fromJust
    in
    case monkey of
        Literal n -> FormLiteral n
        MonkeyOp op ref1 ref2 ->
            FormOp op (getFormula monkeys ref1) (getFormula monkeys ref2)


formulaToString: Formula -> String
formulaToString formula =
    case formula of
        FormLiteral n -> ratStr n
        FormOp op arg1 arg2 ->
            "(" ++ formulaToString arg1 ++ " " ++ opToString op ++ " " ++ formulaToString arg2 ++ ")"
        FormVariable -> "x"


simplifyFormula: Formula -> Formula
simplifyFormula formula =
    case formula of
        FormOp op arg1 arg2 ->
            let
                simp1 = simplifyFormula arg1
                simp2 = simplifyFormula arg2
                (which, simplified) =
                    case (op, simp1, simp2) of
                        (_, FormLiteral n1, FormLiteral n2) ->
                            ("lit", FormLiteral (evalOp op n1 n2))
                        (Mul, FormLiteral val1, FormOp Add arg2_1 arg2_2) ->
                            ("mul", FormOp Add
                                (FormOp Mul (FormLiteral val1) arg2_1)
                                (FormOp Mul (FormLiteral val1) arg2_2))
                        (Mul, FormLiteral val1, FormOp Mul (FormLiteral arg2_1) arg2_2) ->
                            ("mulmul", FormOp Mul
                                (FormLiteral (evalOp op val1 arg2_1))
                                arg2_2)
                        (Sub, FormOp Add (FormLiteral val1_1) arg1_2, FormLiteral val2) ->
                            ("sub", FormOp Add
                                (FormLiteral (evalOp op val1_1 val2))
                                arg1_2)
                        (Add, FormOp Add (FormLiteral val1_1) arg1_2, FormLiteral val2) ->
                            ("add", FormOp Add
                                (FormLiteral (evalOp op val1_1 val2))
                                arg1_2)
                        (Add, FormLiteral val1, FormOp Add (FormLiteral arg2_1) arg2_2) ->
                            ("addadd", FormOp Add
                                (FormLiteral (evalOp op val1 arg2_1))
                                arg2_2)
                        (Div, FormOp Add (FormLiteral val1_1) arg1_2, FormLiteral val2) ->
                            ("divadd", FormOp Add
                                (FormLiteral (evalOp op val1_1 val2))
                                (FormOp Div arg1_2 (FormLiteral val2)))
                        (Div, FormOp Mul (FormLiteral val1_2) arg1_2, FormLiteral val2) ->
                            ("divmul", FormOp Mul
                                (FormLiteral (evalOp op val1_2 val2))
                                arg1_2)
                        (Mul, a1, FormLiteral val2) ->
                            ("mulswap", FormOp Mul (FormLiteral val2) a1)
                        (Sub, a1, a2) ->
                            ("sub2add", FormOp Add
                                a1
                                (FormOp Mul (FormLiteral (BigRational.fromInt -1)) a2))
                        (Mul, FormLiteral val1, a2) ->
                            if BigRational.compare val1 (BigRational.fromInt 1) == EQ then
                                ("one", a2)
                            else
                                ("", FormOp op simp1 simp2)
                        _ -> ("", FormOp op simp1 simp2)
                _ = if which /= "" then
                        --interpolate "({0}): {1} -> {2}" [ which, formulaToString (FormOp op simp1 simp2), formulaToString simplified ]
                        which
                        --|> D.log "simplified"
                    else ""
            in
            simplified
        _ -> formula

evalFormula formula x =
    case formula of
        FormLiteral n -> n
        FormVariable -> x
        FormOp op arg1 arg2 ->
            evalOp op (evalFormula arg1 x) (evalFormula arg2 x)


part1 input =
    let
        monkeys =
            input
                |> parse

        rootFormula = getFormula monkeys "root"

        _ =
            rootFormula
            |> simplifyFormula
            |> formulaToString
            |> D.log "rootFormula"

        simplifyAgain formula rounds =
            let
                simpl = simplifyFormula formula
            in
            if simpl /= formula && rounds > 0 then
                simplifyAgain simpl (rounds-1)
            else
                formula

        final = simplifyAgain rootFormula 20

        _ = final
            |> formulaToString
            |> D.log "final"

        humn = case Dict.get "humn" monkeys of
            Just (Literal n) -> n
            _ -> D.todo "no humn"

    --    _ =
    --        BigRational.sub
    --            (evalFormula rootFormula humn)
    --            (evalFormula final humn)
    --        |> ratStr
    --        |> D.log "delta"

    in
    evalFormula final humn


part2 input =
    let
        monkeys =
            input
                |> parse
    in
    solve monkeys "root"
