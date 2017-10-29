module Board exposing (..)

import BoardPosition exposing (..)
import Matrix
import Matrix.Extra
import Html exposing (Html)


type alias Board =
    Matrix.Matrix Cell


type Cell
    = Alive
    | Dead


constructBlank : Int -> Int -> Board
constructBlank width height =
    Matrix.repeat width height Dead


placeAliveCell : BoardPosition -> Board -> Board
placeAliveCell { x, y } oldBoard =
    Matrix.set x y Alive oldBoard


placeDeadCell : BoardPosition -> Board -> Board
placeDeadCell { x, y } oldBoard =
    Matrix.set x y Dead oldBoard


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


print : Board -> Html msg
print =
    Matrix.Extra.prettyPrint
