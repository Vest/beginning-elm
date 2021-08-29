module Playground exposing (main)

import Html


escapeEarth : Float -> Float -> String -> String
escapeEarth myVelocity mySpeed fuelStatus =
    let
        escapeVelocityInKmPerSec =
            11.186

        orbitalSpeedInKmPerSec =
            7.67

        whereToLand =
            if fuelStatus == "low" then
                "Land on droneship"

            else
                "Land on launchpad"
    in
    if myVelocity > escapeVelocityInKmPerSec then
        "Godspeed"

    else if mySpeed == orbitalSpeedInKmPerSec then
        "Stay in orbit"

    else
        whereToLand


computeSpeed : Float -> Float -> Float
computeSpeed distance time =
    distance / time


computeTime : number -> number -> number
computeTime startTime endTime =
    endTime - startTime


add : number -> number -> number
add a b =
    a + b


multiply : number -> number -> number
multiply c d =
    c * d


divide : Float -> Float -> Float
divide e f =
    e / f


weekday : number -> String
weekday dayInNumber =
    if dayInNumber == 0 then
        "Sunday"

    else if dayInNumber == 1 then
        "Monday"

    else if dayInNumber == 2 then
        "Tuesday"

    else if dayInNumber == 3 then
        "Wednesday"

    else if dayInNumber == 4 then
        "Thursday"

    else if dayInNumber == 5 then
        "Friday"

    else if dayInNumber == 6 then
        "Saturday"

    else
        "Unknown day"


main : Html.Html msg
main =
    weekday 5
        |> Html.text
