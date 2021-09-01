module RippleCarryAdder exposing
    ( Binary
    , andGate
    , digits
    , fullAdder
    , halfAdder
    , inverter
    , orGate
    , rippleCarryAdder
    )

import Array
import Bitwise
import Html exposing (b)
import List exposing (sum)


type alias Binary =
    { d0 : Int
    , d1 : Int
    , d2 : Int
    , d3 : Int
    }


andGate : Int -> Int -> Int
andGate a b =
    Bitwise.and a b


orGate : Int -> Int -> Int
orGate a b =
    Bitwise.or a b


inverter : Int -> Int
inverter a =
    case a of
        0 ->
            1

        1 ->
            0

        _ ->
            -1


halfAdder : Int -> Int -> { carry : Int, sum : Int }
halfAdder a b =
    let
        d =
            orGate a b

        e =
            andGate a b
                |> inverter

        sumDigit =
            andGate d e

        carryOut =
            andGate a b
    in
    { carry = carryOut
    , sum = sumDigit
    }


fullAdder : Int -> Int -> Int -> { carry : Int, sum : Int }
fullAdder a b carryIn =
    let
        firstResult =
            halfAdder b carryIn

        secondResult =
            halfAdder a firstResult.sum

        finalCarry =
            orGate firstResult.carry secondResult.carry
    in
    { carry = finalCarry
    , sum = secondResult.sum
    }


rippleCarryAdder : Int -> Int -> Int -> Int
rippleCarryAdder a b carryIn =
    let
        firstSignal =
            extractDigits a

        secondSignal =
            extractDigits b

        firstResult =
            fullAdder firstSignal.d3 secondSignal.d3 carryIn

        secondResult =
            fullAdder firstSignal.d2 secondSignal.d2 firstResult.carry

        thirdResult =
            fullAdder firstSignal.d1 secondSignal.d1 secondResult.carry

        finalResult =
            fullAdder firstSignal.d0 secondSignal.d0 thirdResult.carry
    in
    [ finalResult, thirdResult, secondResult, firstResult ]
        |> List.map .sum
        |> (::) finalResult.carry
        |> numberFromDigits


extractDigits : Int -> Binary
extractDigits number =
    String.fromInt number
        |> String.padLeft 4 '0'
        |> String.split ""
        |> List.map stringToInt
        |> Array.fromList
        |> arrayToRecord


stringToInt : String -> Int
stringToInt string =
    String.toInt string
        |> Maybe.withDefault -1


arrayToRecord : Array.Array Int -> Binary
arrayToRecord array =
    let
        firstElement =
            Array.get 0 array
                |> Maybe.withDefault -1

        secondElement =
            Array.get 1 array
                |> Maybe.withDefault -1

        thirdElement =
            Array.get 2 array
                |> Maybe.withDefault -1

        fourthElement =
            Array.get 3 array
                |> Maybe.withDefault -1
    in
    Binary firstElement secondElement thirdElement fourthElement


numberFromDigits : List Int -> Int
numberFromDigits digitsList =
    List.foldl (\digit number -> digit + 10 * number) 0 digitsList


digits : Int -> List Int
digits number =
    if number == 0 then
        []

    else
        digits (number // 10) ++ [ remainderBy 10 number ]
