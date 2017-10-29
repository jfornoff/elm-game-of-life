module Suite exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Board
import BoardPosition exposing (BoardPosition)


suite : Test
suite =
    describe "Game of Life"
        [ describe "Evolution" evolutionTests
        ]


evolutionTests : List Test
evolutionTests =
    [ describe "1x1 board" <|
        let
            blankBoard =
                Board.constructBlank 1 1

            onlySpace =
                BoardPosition 0 0
        in
            [ describe "with an unpopulated space"
                [ test "does nothing" <|
                    \_ ->
                        let
                            subject =
                                blankBoard |> Board.evolve
                        in
                            subject |> Expect.equal blankBoard
                , test "has a dead cell" <|
                    \_ ->
                        blankBoard
                            |> Board.isDeadCell onlySpace
                            |> Expect.true "Expected cell to be dead"
                ]
            , describe "with an alive cell"
                [ test "the cell dies" <|
                    \_ ->
                        let
                            board =
                                blankBoard |> Board.placeAliveCell onlySpace

                            subject =
                                board |> Board.evolve
                        in
                            subject
                                |> Board.isDeadCell onlySpace
                                |> Expect.true "Expected cell to be dead"
                ]
            , describe "with a dead cell"
                [ test "it stays dead" <|
                    \_ ->
                        let
                            board =
                                blankBoard |> Board.placeDeadCell onlySpace

                            subject =
                                board |> Board.evolve
                        in
                            subject
                                |> Board.isDeadCell onlySpace
                                |> Expect.true "Expected cell to be dead"
                ]
            ]
    , describe "3x3 board" <|
        let
            blankBoard =
                Board.constructBlank 3 3
        in
            [ describe "alive cell in center" <|
                let
                    center =
                        BoardPosition 1 1

                    boardWithAliveCell =
                        blankBoard
                            |> Board.placeAliveCell center
                in
                    [ test "with no neighbors dies of solitude" <|
                        \_ ->
                            boardWithAliveCell
                                |> Board.evolve
                                |> Board.isDeadCell center
                                |> Expect.true "Expected center to die"
                    , test "with one neighbor dies of solitude" <|
                        \_ ->
                            boardWithAliveCell
                                |> Board.placeAliveCell (BoardPosition.leftOf center)
                                |> Board.evolve
                                |> Board.isDeadCell center
                                |> Expect.true "Expected center to die"
                    , test "with two neighbors, survives" <|
                        \_ ->
                            boardWithAliveCell
                                |> Board.placeAliveCell (BoardPosition.leftOf center)
                                |> Board.placeAliveCell (BoardPosition.rightOf center)
                                |> Board.evolve
                                |> Board.isAliveCell center
                                |> Expect.true "Expected center to stay alive"
                    , test "with three neighbors, survives" <|
                        \_ ->
                            boardWithAliveCell
                                |> Board.placeAliveCell (BoardPosition.leftOf center)
                                |> Board.placeAliveCell (BoardPosition.rightOf center)
                                |> Board.placeAliveCell (BoardPosition.below center)
                                |> Board.evolve
                                |> Board.isAliveCell center
                                |> Expect.true "Expected center to stay alive"
                    , test "with four neighbors, dies" <|
                        \_ ->
                            boardWithAliveCell
                                |> Board.placeAliveCell (BoardPosition.leftOf center)
                                |> Board.placeAliveCell (BoardPosition.rightOf center)
                                |> Board.placeAliveCell (BoardPosition.below center)
                                |> Board.placeAliveCell (BoardPosition.above center)
                                |> Board.evolve
                                |> Board.isAliveCell center
                                |> Expect.false "Expected center to die"
                    ]
            , describe "dead cell in the center of the board" <|
                let
                    center =
                        BoardPosition 1 1
                in
                    [ test "with three alive neighbors comes alive" <|
                        \_ ->
                            blankBoard
                                |> Board.placeAliveCell (BoardPosition.leftOf center)
                                |> Board.placeAliveCell (BoardPosition.rightOf center)
                                |> Board.placeAliveCell (BoardPosition.above center)
                                |> Board.evolve
                                |> Board.isAliveCell center
                                |> Expect.true "Expected center to come alive"
                    ]
            ]
    ]
