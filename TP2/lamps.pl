:-use_module(library(clpfd)).
:-use_module(library(lists)).

solve :- 
    Board = [[4, 6, 6, 5],[5, 2, 6, 4],[4, 6, 6, 6],[4, 6, 6, 4]],
    length(TopRow, 4), domain(TopRow, 1, 4),
    length(RightRow, 4), domain(RightRow, 1, 4),
    length(BottomRow, 4), domain(BottomRow, 1, 4),
    length(LeftRow, 4), domain(LeftRow, 1, 4),
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
    elementVal(TopRow, Index_j, VTop), 
    elementVal(BottomRow, Index_j, VBottom),
    elementVal(RightRow, Index_i, VRight),
    elementVal(LeftRow, Index_i, VLeft),
    Sum #= VTop + VBottom + VRight + VLeft,
    element(Index_i, Board, Row),
    element(Index_j, Row, Sum),
    New_Index is Index_j + 1,
    restrict_board_row_elems(Index_i, New_Index, Board).

elementVal(Row, Index, Res) :-
    Elem in 1..8,
    element(Index, Row, Elem),
    Index #>= Elem #<=> Res #= 1,
    Index #< Elem #<=> Res #= 0.

