module FuzzTests exposing (addOneTests)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Http exposing (Expect)
import Test exposing (..)


addOneTests : Test
addOneTests =
    describe "addOne"
        [ fuzz int "adds 1 to any integer" <|
            \num ->
                addOne num |> Expect.equal (num + 1)
        , test "when 1 is added to 2, the result is 3" <|
            \_ ->
                addOne 2 |> Expect.equal 3
        ]


addTests : Test
addTests =
    describe "add"
        [ fuzz2 int int "adds tw given integers" <|
            \num1 num2 ->
                add num1 num2
                    |> Expect.equal (num1 + num2)
        ]


addOne : Int -> Int
addOne x =
    1 + x


add : Int -> Int -> Int
add x y =
    x + y
