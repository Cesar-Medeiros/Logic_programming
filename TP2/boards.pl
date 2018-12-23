:-use_module(library(lists)).
:-use_module(library(between)).

board_2x2([
    cell(1, 1, 0),
    cell(1, 2, 0),
    cell(2, 1, 0),
    cell(2, 2, 0)
]).

board_4x4([
        cell(1, 1, 4),
        cell(1, 2, 6),
        cell(1, 3, 6),
        cell(1, 4, 5),

        cell(2, 1, 5),
        cell(2, 2, 2),
        cell(2, 3, 6),
        cell(2, 4, 4),

        cell(3, 1, 4),
        cell(3, 2, 6),
        cell(3, 3, 6),
        cell(3, 4, 6),

        cell(4, 1, 4),
        cell(4, 2, 6),
        cell(4, 3, 6),
        cell(4, 4, 4)
    ]).

board_5x5([
        cell(1,1,3),
        cell(1,2,3),
        cell(1,3,2),
        cell(1,4,5),
        cell(1,5,4),

        cell(2,1,5),
        cell(2,2,3),
        cell(2,3,5),
        cell(2,4,3),
        cell(2,5,5),

        cell(3,1,5),
        cell(3,2,6),
        cell(3,3,2),
        cell(3,4,4),
        cell(3,5,6),

        cell(4,1,5),
        cell(4,2,2),
        cell(4,3,2),
        cell(4,4,2),
        cell(4,5,4),

        cell(5,1,4),
        cell(5,2,5),
        cell(5,3,4),
        cell(5,4,3),
        cell(5,5,2)
    ]).

getCell(Board, Row, Col, Content) :-
    BoardCell-_ = Board,
    member(cell(Row, Col, Content), BoardCell).

varBoard(Board, Dim) :-
    findall(cell(R, C, _), (between(1, Dim, R), between(1, Dim, C)),  Board).