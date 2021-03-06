{-
   The Playground module is used to experiment with
   various concepts in Elm programming language.of
-}


module Playground exposing
    ( Greeting(..)
    , add
    , main
    , sayHello
    )

import Html
import MyList exposing (MyList)


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


weekday : Int -> String
weekday dayInNumber =
    case dayInNumber of
        0 ->
            "Sunday"

        1 ->
            "Monday"

        2 ->
            "Tuesday"

        3 ->
            "Wednesday"

        4 ->
            "Thursday"

        5 ->
            "Friday"

        6 ->
            "Saturday"

        _ ->
            "Unknown day"


hashtag : Int -> String
hashtag dayInNumber =
    case weekday dayInNumber of
        "Sunday" ->
            "#SinDay"

        "Monday" ->
            "#MondayBlues"

        "Tuesday" ->
            "#TakeMeBackTuesday"

        "Wednesday" ->
            "#HumpDay"

        "Thursday" ->
            "#ThrowbackThursday"

        "Friday" ->
            "#FlashbackFriday"

        "Saturday" ->
            "#Caturday"

        _ ->
            "#Whatever"


descending a b =
    case compare a b of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            EQ


evilometer character1 character2 =
    case ( character1, character2 ) of
        ( "Joffrey", "Ramsay" ) ->
            LT

        ( "Joffrey", "Night King" ) ->
            LT

        ( "Ramsay", "Joffrey" ) ->
            GT

        ( "Ramsay", "Night King" ) ->
            LT

        ( "Night King", "Joffrey" ) ->
            GT

        ( "Night King", "Ramsay" ) ->
            GT

        _ ->
            GT


type Greeting
    = Howdy
    | Hola


sayHello : Greeting -> String
sayHello greeting =
    case greeting of
        Howdy ->
            "How y'all doin'?"

        Hola ->
            "Hola amigo!"


list1 : MyList.MyList a
list1 =
    MyList.Empty


list2 : MyList.MyList number
list2 =
    MyList.Node 9 MyList.Empty


main : Html.Html msg
main =
    MyList.isEmpty list2
        |> Debug.toString
        |> Html.text
