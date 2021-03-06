module FuzzTests exposing
    ( addOneTests
    , addTests
    , flipTests
    , listLengthTests
    , multiplyFloatTests
    , pizzaLeftTests
    , stringTests
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
        , fuzz2 (floatRange -1.0 1.0) int "multiplies given numbers in range" <|
            \x y ->
                multiplyFloat x y
                    |> Expect.within (Absolute 0.000000001) (x * toFloat y)
        ]


pizzaLeft : Float -> Float -> Float
pizzaLeft eatenPercent totalSlices =
    totalSlices - (eatenPercent * totalSlices)


pizzaLeftTests : Test
pizzaLeftTests =
    describe "pizzaLeft"
        [ fuzz2 percentage float "returns remaining pizza slices" <|
            \eaten total ->
                pizzaLeft eaten total
                    |> Expect.within (Absolute 0.00001) (total * (1 - eaten))
        ]


stringTests : Test
stringTests =
    describe "The String module"
        [ describe "String.reverse"
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    palindrome
                        |> String.reverse
                        |> Expect.equal palindrome
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]


listLengthTests : Test
listLengthTests =
    describe "List.length"
        [ fuzz (list int) "never returns a negative value" <|
            \intList ->
                intList
                    |> List.length
                    |> Expect.atLeast 0
        ]
