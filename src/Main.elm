module Main exposing (..)

import Html exposing (..)
import Board


type alias Model =
    { board : Board.BoardState }


type Msg
    = Noop


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
    Model <| Board.constructBlank 15 15


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ Board.print model.board
        ]
