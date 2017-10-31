module Main exposing (..)

import Html exposing (..)
import Board exposing (Board)
import BoardPosition exposing (BoardPosition)
import Time
import Html.Events exposing (..)


type alias Model =
    { board : Board, evolutionRunning : Bool }


type Msg
    = BoardMsg Board.Msg
    | StartEvolution
    | StopEvolution


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    initialModel ! []


initialModel : Model
initialModel =
    let
        initialBoard =
            Board.constructBlank 15 15
                |> Board.placeAliveCell (BoardPosition 6 6)
                |> Board.placeAliveCell (BoardPosition 7 7)
                |> Board.placeAliveCell (BoardPosition 8 7)
                |> Board.placeAliveCell (BoardPosition 8 8)
    in
        { board = initialBoard, evolutionRunning = False }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartEvolution ->
            { model | evolutionRunning = True } ! []

        StopEvolution ->
            { model | evolutionRunning = False } ! []

        BoardMsg boardMsg ->
            { model | board = Board.update boardMsg model.board } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.evolutionRunning then
        Time.every Time.second (\_ -> BoardMsg Board.Evolve)
    else
        Sub.none


view : Model -> Html Msg
view model =
    div []
        [ Html.map BoardMsg <| Board.view model.board
        , evolutionToggle model
        ]


evolutionToggle : Model -> Html Msg
evolutionToggle model =
    if model.evolutionRunning then
        button [ onClick StopEvolution ] [ text "Stop" ]
    else
        button [ onClick StartEvolution ] [ text "Start" ]
