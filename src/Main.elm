module Main exposing (..)

import Html exposing (..)
import Board exposing (Board, Cell(..))
import BoardPosition exposing (BoardPosition)
import Time


type alias Model =
    { board : Board, evolutionRunning : Bool }


type Msg
    = EvolveBoard


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
        { board = initialBoard, evolutionRunning = True }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EvolveBoard ->
            { model | board = Board.evolve model.board } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.evolutionRunning then
        Time.every Time.second (\_ -> EvolveBoard)
    else
        Sub.none


view : Model -> Html Msg
view model =
    div []
        [ Board.viewBoard model.board
        ]
