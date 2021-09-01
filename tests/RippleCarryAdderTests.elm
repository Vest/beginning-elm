module RippleCarryAdderTests exposing (andGateTests, inverterTests, orGateTests)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Http exposing (Expect)
import RippleCarryAdder exposing (..)
import Test exposing (..)


inverterTests =
    describe "Inverter"
        [ test "output is 0 when the input is 1" <|
            \_ ->
                inverter 0
                    |> Expect.equal 1
        , test "output is 1 when the input is 0" <|
            \_ ->
                inverter 1
                    |> Expect.equal 0
        ]


andGateTests =
    describe "AND gate"
        [ test "output is 0 when both inputs are 0" <|
            \_ ->
                andGate 0 0
                    |> Expect.equal 0
        , test "output is 0 when the first input is 0" <|
            \_ ->
                andGate 1 0
                    |> Expect.equal 0
        , test "output is 0 when the second input is 0" <|
            \_ ->
                andGate 0 1
                    |> Expect.equal 0
        , test "output is 1 when both inputs are 1" <|
            \_ ->
                andGate 1 1
                    |> Expect.equal 1
        ]


orGateTests =
    describe "OR gate"
        [ test "output is 0 when both inputs are 0" <|
            \_ ->
                orGate 0 0
                    |> Expect.equal 0
        , test "output is 1 when the first input is 0" <|
            \_ ->
                orGate 1 0
                    |> Expect.equal 1
        , test "output is 1 when the second input is 0" <|
            \_ ->
                orGate 0 1
                    |> Expect.equal 1
        , test "output is 1 when both inputs are 1" <|
            \_ ->
                orGate 1 1
                    |> Expect.equal 1
        ]
