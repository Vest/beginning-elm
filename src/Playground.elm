module Playground exposing (main)

import Html


escapeEarth : Float -> Float -> String
escapeEarth myVelocity mySpeed =
    if myVelocity > 11.186 then
        "Godspeed"

    else if mySpeed == 7.67 then
        "Stay in orbit"

    else
        "Come back"


computeSpeed : Float -> Float -> Float
computeSpeed distance time =
    distance / time


computeTime : number -> number -> number
computeTime startTime endTime =
    endTime - startTime


add a b =
    a + b


multiply c d =
    c * d


divide e f =
    e / f


main : Html.Html msg
main =
    divide 30 10
        |> multiply 10
        |> add 5
        |> String.fromFloat
        |> Html.text
