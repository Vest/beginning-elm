module RippleCarryAdder exposing (andGate)

import Bitwise


andGate : Int -> Int -> Int
andGate a b =
    Bitwise.and a b
