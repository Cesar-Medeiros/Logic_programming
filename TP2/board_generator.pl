:- consult(lamps).
:- consult(display).

generateRandomList(List, Length) :-
    findall(Val, (between(1, Length, _), random(0, Length, Val)), List).


generateBoard(Dim, TopRow, BottomRow, RightRow, LeftRow, Board):-
    generateRandomList(TopRow,    Dim),
    generateRandomList(BottomRow, Dim),
    generateRandomList(RightRow,  Dim),
    generateRandomList(LeftRow,   Dim),

    varBoard(Board, Dim),
    restrict_board(Board, Dim, TopRow, BottomRow, RightRow, LeftRow).


generateAndSolve(Dim) :-
    generateBoard(Dim, TopRow, BottomRow, RightRow, LeftRow, Board),
    printBoard(Board, TopRow, BottomRow, RightRow, LeftRow),
    nl,nl,
    printBoardRepresentation(Board),
    nl,nl,
    solve(Board, TopRow1, BottomRow1, RightRow1, LeftRow1, [anti_first_fail, up, bisect], Time),
    write(Time),write(' ms'),nl,
    printBoard(Board, TopRow1, BottomRow1, RightRow1, LeftRow1).