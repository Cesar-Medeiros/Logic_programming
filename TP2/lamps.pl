:-use_module(library(clpfd)).
:-use_module(library(lists)).

solve(Board) :- 
    length(TopRow, 4),
    length(RightRow, 4),
    length(BottomRow, 4),
    length(LeftRow, 4),
    restrict_board(Board, TopRow, RightRow, BottomRow, LeftRow),

    append(TopRow, RightRow, TopAndRight),
    append(TopAndRight, BottomRow, ExceptLeft),
    append(ExceptLeft, LeftRow, List),
    labeling([], List),
    write(List).

restrict_board(Board, TopRow, BottomRow, RightRow, LeftRow) :-
    restrict_board_rows(Board, 0, TopRow, BottomRow, RightRow, LeftRow).

restrict_board_rows(Board, Index, TopRow, BottomRow, RightRow, LeftRow) :-
    restrict_board_row_elems(Index, 0, Board, TopRow, BottomRow, RightRow, LeftRow),
    New_Index is Index + 1,
    restrict_board_rows(Board, New_Index, TopRow,  BottomRow, RightRow, LeftRow).


restrict_board_row_elems(Index_i, Index_j, Board, TopRow, BottomRow, RightRow, LeftRow) :-
    element(TopRow, Index_j, Top),
    element(BottomRow, Index_j, Bottom),
    element(RightRow, Index_i, Right),
    element(LeftRow, Index_i, Left),
    Sum #= Top + Bottom + Right + Left,
    element(Index_i, Board, Row),
    element(Index_j, Row, Sum),
    New_Index is Index_j + 1,
    restrict_board_row_elems(Index_i, New_Index, Board).

