module FuzzTests exposing
    ( addOneTests
    , addTests
    , flipTests
    , multiplyFloatTests
    )

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (..)
import Http exposing (Expect)
import Random exposing (maxInt, minInt)
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
        , fuzzWith { runs = 200 } int "adds 1 to the given integer" <|
            \num ->
                addOne num |> Expect.equal (num + 1)
        , fuzz (intRange -100 100) "adds 1 to the given integer in a range" <|
            \num ->
                addOne num |> Expect.equal (num + 1)
        , fuzz (intRange minInt maxInt) "adds 1 to all integers" <|
            \num ->
                addOne num |> Expect.equal (num + 1)
        , fuzz frequencyFuzzer "adds 1 via fuzzier" <|
            \num ->
                addOne num |> Expect.equal (num + 1)
        ]


addTests : Test
addTests =
    describe "add"
        [ fuzz2 int int "adds two given integers" <|
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


frequencyFuzzer : Fuzzer Int
frequencyFuzzer =
    frequency
        [ ( 70, constant 7 )
        , ( 12, intRange 8 9 )
        , ( 6, constant 6 )
        , ( 9, intRange 2 4 )
        , ( 1, constant 5 )
        , ( 1, constant 1 )
        , ( 1, constant 10 )
        ]


flip : Bool -> Bool
flip x =
    not x


flipTests : Test
flipTests =
    describe "flip"
        [ fuzz bool "negates the given boolean value" <|
            \value ->
                flip value |> Expect.equal (not value)
        ]


multiplyFloat : Float -> Int -> Float
multiplyFloat x y =
    x * toFloat y


multiplyFloatTests : Test
multiplyFloatTests =
    describe "multiplyFloat"
        [ fuzz2 float int "multiplies given numbers" <|
            \x y ->
                multiplyFloat x y
                    |> Expect.within (Absolute 0.000000001) (x * toFloat y)
        ]
