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
        ]


addOne : Int -> Int
addOne x =
    1 + x
