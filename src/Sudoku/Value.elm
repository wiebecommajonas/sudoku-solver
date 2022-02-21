module Sudoku.Value exposing (..)

import List.Extra


type Value
    = None
    | V1
    | V2
    | V3
    | V4
    | V5
    | V6
    | V7
    | V8
    | V9


values : List Value
values =
    [ V1, V2, V3, V4, V5, V6, V7, V8, V9 ]


toInt : Value -> Int
toInt value =
    List.Extra.findIndex ((==) value) values |> Maybe.withDefault -1 |> (+) 1


toString : Value -> String
toString value =
    toInt value |> String.fromInt


fromInt : Int -> Maybe Value
fromInt number =
    List.Extra.getAt (number - 1) values
