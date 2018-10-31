% board(+Board)
%   Select a Board to start the game with
%board(Board) :- board_begin(Board).
createBoard([], 0, _).
createBoard([L|T], Rows, Cols) :-
    createLine(L, Cols),
    Rows1 is Rows-1,
    createBoard(T, Rows1, Cols).

createLine([], 0).
createLine([empty|L], N) :-
    N1 is N-1,
    createLine(L, N1).


