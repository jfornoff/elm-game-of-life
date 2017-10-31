module Board
    exposing
        ( Board
        , Msg(..)
        , view
        , update
        , placeAliveCell
        , placeDeadCell
        , isAliveCell
        , isDeadCell
        , evolve
        , constructBlank
        )

import BoardPosition exposing (..)
import Matrix
import Matrix.Extra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array
import List.Extra


type alias Board =
    Matrix.Matrix Cell


type Cell
    = Alive
    | Dead


type Msg
    = Evolve
    | KillCell BoardPosition
    | ReviveCell BoardPosition


constructBlank : Int -> Int -> Board
constructBlank width height =
    Matrix.repeat width height Dead


placeAliveCell : BoardPosition -> Board -> Board
placeAliveCell { x, y } oldBoard =
    Matrix.set x y Alive oldBoard


placeDeadCell : BoardPosition -> Board -> Board
placeDeadCell { x, y } oldBoard =
    Matrix.set x y Dead oldBoard


placeCell : Cell -> BoardPosition -> Board -> Board
placeCell cell { x, y } oldBoard =
    Matrix.set x y cell oldBoard


isDeadCell : BoardPosition -> Board -> Bool
isDeadCell { x, y } board =
    board
        |> Matrix.get x y
        |> Maybe.map (\space -> space == Dead)
        |> Maybe.withDefault False


isAliveCell : BoardPosition -> Board -> Bool
isAliveCell { x, y } board =
    board
        |> Matrix.get x y
        |> Maybe.map (\space -> space == Alive)
        |> Maybe.withDefault False


evolve : Board -> Board
evolve board =
    board
        |> Matrix.indexedMap (evolveCell board)


evolveDead : List Cell -> Cell
evolveDead neighbors =
    neighbors
        |> List.filter (\space -> space == Alive)
        |> List.length
        |> \numberOfAliveNeighbors ->
            if numberOfAliveNeighbors == 3 then
                Alive
            else
                Dead


evolveAlive : List Cell -> Cell
evolveAlive neighbors =
    neighbors
        |> List.filter (\space -> space == Alive)
        |> List.length
        |> \numberOfAliveNeighbors ->
            if numberOfAliveNeighbors < 2 then
                Dead
            else if numberOfAliveNeighbors < 4 then
                Alive
            else
                Dead


evolveCell : Board -> Int -> Int -> Cell -> Cell
evolveCell board x y self =
    let
        neighbors =
            Matrix.Extra.neighbours x y board
    in
        case self of
            Dead ->
                evolveDead neighbors

            Alive ->
                evolveAlive neighbors



-- Update


update : Msg -> Board -> Board
update msg oldBoard =
    case msg of
        Evolve ->
            evolve oldBoard

        KillCell position ->
            placeDeadCell position oldBoard

        ReviveCell position ->
            placeAliveCell position oldBoard



-- View


view : Board -> Html Msg
view board =
    board
        |> Matrix.indexedMap viewTableCell
        |> viewRows
        |> table []


viewTableCell : Int -> Int -> Cell -> Html Msg
viewTableCell x y cell =
    case cell of
        Alive ->
            td [ class "cell alive", onClick <| KillCell (BoardPosition x y) ] []

        Dead ->
            td [ class "cell", onClick <| ReviveCell (BoardPosition x y) ]
                []


viewRows : Matrix.Matrix (Html msg) -> List (Html msg)
viewRows cellMatrix =
    cellMatrix.data
        |> Array.toList
        |> List.Extra.groupsOf (Matrix.width cellMatrix)
        |> List.map (tr [])
