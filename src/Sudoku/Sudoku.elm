module Sudoku.Sudoku exposing (..)

import List.Extra
import Sudoku.Value exposing (Value(..), values)


type alias Sudoku =
    List Value


empty : Sudoku
empty =
    List.repeat 81 None


rowIndex : Int -> Int
rowIndex index =
    index // 9


colIndex : Int -> Int
colIndex =
    modBy 9


boxIndex : Int -> Int
boxIndex index =
    modBy 3 (index // 3) + (index // 27) * 3


area : (Int -> Int) -> Int -> Sudoku -> List ( Int, Value )
area indexer index sudoku =
    List.indexedMap (\i value -> ( i, value )) sudoku
        |> List.filter (\( i, value ) -> indexer i == index)


col : Int -> Sudoku -> List ( Int, Value )
col =
    area colIndex


row : Int -> Sudoku -> List ( Int, Value )
row =
    area rowIndex


box : Int -> Sudoku -> List ( Int, Value )
box =
    area boxIndex


nextEmpty : Sudoku -> Maybe Int
nextEmpty =
    List.Extra.findIndex ((==) None)


validWith : Value -> Int -> Sudoku -> Bool
validWith value index sudoku =
    let
        validate segment =
            not <| List.any (\( i, val ) -> val == value && not (i == index)) segment
    in
    validate (col (colIndex index) sudoku)
        && validate (row (rowIndex index) sudoku)
        && validate (box (boxIndex index) sudoku)


solveHelper : Int -> Sudoku -> List Sudoku -> ( Bool, List Sudoku )
solveHelper counter sudoku log =
    case nextEmpty sudoku of
        Just i ->
            let
                value =
                    List.Extra.getAt counter values
            in
            case value of
                Just val ->
                    if validWith val i sudoku then
                        let
                            newSudoku =
                                List.Extra.setAt i val sudoku

                            newLog =
                                newSudoku :: log
                        in
                        case solveHelper 0 newSudoku newLog of
                            ( True, llog ) as result ->
                                result

                            ( False, llog ) ->
                                let
                                    resetSudoku =
                                        List.Extra.setAt i None sudoku

                                    resetLog =
                                        resetSudoku :: llog
                                in
                                solveHelper (counter + 1) resetSudoku resetLog

                    else
                        solveHelper (counter + 1) sudoku log

                Nothing ->
                    ( False, log )

        Nothing ->
            ( True, log )


solve : Sudoku -> Maybe (List Sudoku)
solve sudoku =
    case solveHelper 0 sudoku [] of
        ( True, log ) ->
            Just <| List.reverse log

        _ ->
            Nothing
