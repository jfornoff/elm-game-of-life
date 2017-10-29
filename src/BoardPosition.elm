module BoardPosition exposing (..)


type alias BoardPosition =
    { x : Int, y : Int }


leftOf : BoardPosition -> BoardPosition
leftOf { x, y } =
    BoardPosition (x - 1) y


rightOf : BoardPosition -> BoardPosition
rightOf { x, y } =
    BoardPosition (x + 1) y


below : BoardPosition -> BoardPosition
below { x, y } =
    BoardPosition x (y - 1)


above : BoardPosition -> BoardPosition
above { x, y } =
    BoardPosition x (y + 1)
