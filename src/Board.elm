module Board exposing (..)

import BoardPosition exposing (..)
import Matrix
import Matrix.Extra
import Html exposing (Html)


type alias BoardState =
    Matrix.Matrix BoardSpace


type BoardSpace
    = Alive
    | Dead


constructBlank : Int -> Int -> BoardState
constructBlank width height =
    Matrix.repeat width height Dead


placeAliveCell : BoardPosition -> BoardState -> BoardState
placeAliveCell { x, y } oldBoard =
    Matrix.set x y Alive oldBoard


placeDeadCell : BoardPosition -> BoardState -> BoardState
placeDeadCell { x, y } oldBoard =
    Matrix.set x y Dead oldBoard


isDeadCell : BoardPosition -> BoardState -> Bool
isDeadCell { x, y } board =
    board
        |> Matrix.get x y
        |> Maybe.map (\space -> space == Dead)
        |> Maybe.withDefault False


isAliveCell : BoardPosition -> BoardState -> Bool
isAliveCell { x, y } board =
    board
        |> Matrix.get x y
        |> Maybe.map (\space -> space == Alive)
        |> Maybe.withDefault False


evolve : BoardState -> BoardState
evolve board =
    board
        |> Matrix.indexedMap (evolveCell board)


evolveDead : List BoardSpace -> BoardSpace
evolveDead neighbors =
    neighbors
        |> List.filter (\space -> space == Alive)
        |> List.length
        |> \numberOfAliveNeighbors ->
            if numberOfAliveNeighbors == 3 then
                Alive
            else
                Dead


evolveAlive : List BoardSpace -> BoardSpace
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


evolveCell : BoardState -> Int -> Int -> BoardSpace -> BoardSpace
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


print : BoardState -> Html msg
print =
    Matrix.Extra.prettyPrint
