module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (button, div, li, main_, nav, span, text, ul)
import Html.Attributes exposing (class, href, rel, target)
import Html.Events exposing (onClick)
import Json.Decode as Dec
import List.Extra as Lex
import Maybe exposing (withDefault)
import Process
import Sudoku.Sudoku exposing (Sudoku, area, boxIndex, colIndex, rowIndex, solve)
import Sudoku.Value
import Task


type alias Model =
    { sudoku : Sudoku
    , selected : Maybe Int
    , log : List Sudoku
    }


type Msg
    = None
    | NumberPressed Int
    | NumberCleared
    | Select Int
    | Reset
    | Solve
    | ShowLog


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sudoku = Sudoku.Sudoku.empty, selected = Nothing, log = [] }, Cmd.none )


showLog : () -> Cmd Msg
showLog _ =
    Process.sleep 10
        |> Task.perform (\_ -> ShowLog)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init ()

        NumberPressed number ->
            case model.selected of
                Just index ->
                    ( { model | sudoku = Lex.setAt index (Sudoku.Value.fromInt number |> withDefault Sudoku.Value.None) model.sudoku }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NumberCleared ->
            case model.selected of
                Just index ->
                    ( { model | sudoku = Lex.setAt index Sudoku.Value.None model.sudoku }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Select index ->
            case Maybe.map ((==) index) model.selected of
                Just True ->
                    ( { model | selected = Nothing }, Cmd.none )

                _ ->
                    ( { model | selected = Just index }, Cmd.none )

        Solve ->
            let
                solved =
                    case solve model.sudoku of
                        Just s ->
                            s

                        _ ->
                            [ model.sudoku ]
            in
            ( { model | log = solved }, showLog () )

        ShowLog ->
            case model.log of
                l :: ls ->
                    ( { model | sudoku = l, log = ls }, showLog () )

                [] ->
                    ( model, Cmd.none )

        None ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Sudoku solver"
    , body =
        [ nav []
            [ ul [] [ li [] [ span [ class "highlight" ] [ text "Sudoku Solver" ] ] ]
            , ul []
                [ li [] [ button [ onClick <| Solve ] [ text "Solve" ] ]
                , li [] [ button [ onClick <| Reset ] [ text "Reset" ] ]
                ]
            ]
        , main_ []
            [ div [ class "sudoku-grid" ] <|
                List.indexedMap
                    (\index field ->
                        div
                            [ class "sudoku-cell"
                            , class <|
                                case Maybe.map ((==) index) model.selected of
                                    Just True ->
                                        "sudoku-cell--selected"

                                    _ ->
                                        ""
                            , class <|
                                if modBy 3 index == 0 && not (modBy 9 index == 0) then
                                    "sudoku-cell--border-left"

                                else
                                    ""
                            , class <|
                                if modBy 3 (index // 9) == 0 && not (modBy 9 (index // 9) == 0) then
                                    "sudoku-cell--border-top"

                                else
                                    ""
                            , onClick <| Select index
                            ]
                            [ span []
                                [ text <|
                                    case field of
                                        Sudoku.Value.None ->
                                            ""

                                        _ ->
                                            Sudoku.Value.toString field
                                ]
                            ]
                    )
                    model.sudoku
            , div [ class "cc" ]
                [ span [] [ text "by " ], Html.a [ target "_blank", rel "noopener noreferrer", href "https://github.com/wiebecommajonas" ] [ text "wiebecommajonas" ] ]
            ]
        ]
    }


keyDecoder : Dec.Decoder Msg
keyDecoder =
    Dec.map toKey (Dec.field "key" Dec.string)


toKey : String -> Msg
toKey keyValue =
    if keyValue == "Backspace" then
        NumberCleared

    else
        case String.left 1 keyValue |> String.toInt of
            Just num ->
                NumberPressed num

            _ ->
                None


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> onKeyDown keyDecoder
        }
