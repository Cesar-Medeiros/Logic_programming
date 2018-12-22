:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(random)).

:- consult(boards).
:- consult(display).

full(Dim) :-
    generateBoard(Dim, TopRow, BottomRow, RightRow, LeftRow, Board),
    printBoard(Board-[Dim, Dim], TopRow, BottomRow, RightRow, LeftRow),
    nl,nl,
    printBoardRepresentation(Board),
    nl,nl,
    solve(Board, TopRow1, BottomRow1, RightRow1, LeftRow1),
    printBoard(Board-[Dim, Dim], TopRow1, BottomRow1, RightRow1, LeftRow1).

generateRandomList(List, Length) :-
    findall(Val, (between(1, Length, _), random(0, Length, Val)), List).



generateBoard(Dim, TopRow, BottomRow, RightRow, LeftRow, Board):-
    generateRandomList(TopRow,    Dim),
    generateRandomList(BottomRow, Dim),
    generateRandomList(RightRow,  Dim),
    generateRandomList(LeftRow,   Dim),

    varBoard(Board, Dim),
    restrict_board(Board, Dim, TopRow, BottomRow, RightRow, LeftRow).

solve(Board, TopRow, BottomRow, RightRow, LeftRow) :-
    statistics(runtime, [T1|_]),
    getBoardDim(Board, Dim),
    
    length(TopRow, Dim),      domain(TopRow, 0, Dim),
    length(RightRow, Dim),    domain(RightRow, 0, Dim),
    length(BottomRow, Dim),   domain(BottomRow, 0, Dim),
    length(LeftRow, Dim),     domain(LeftRow, 0, Dim),

    restrict_board(Board, Dim, TopRow, BottomRow, RightRow, LeftRow),

    append([TopRow, BottomRow, RightRow, LeftRow], List),
    labeling([], List),
    write('Result: '),
    write(List),
    nl,

    statistics(runtime, [T2|_]),
    T3 is T2 - T1,
    write('Time: '),
    write(T3),
    write(' ms'),
    nl,nl.

getBoardDim(Board, Dim) :-
    length(Board, Size),
    Dim is floor(sqrt(Size)).

restrict_board([], _, _, _, _, _).

restrict_board([Cell | RestBoard], Dim, TopRow, BottomRow, RightRow, LeftRow) :-

    Cell = cell(Row, Col, Val),

    element(Col, TopRow,    TRVal),
    element(Col, BottomRow, BRVal),

    element(Row, RightRow,  RRVal),
    element(Row, LeftRow,   LRVal),

    Row #=< TRVal #<=> ResTR,
    Row #>= (Dim + 1 - BRVal) #<=> ResBR,

    Col #=< LRVal #<=> ResLR,
    Col #>= (Dim + 1 - RRVal) #<=> ResRR,

    LRD1 is Row - Col,
    ((LRD1 > 0,  LRD1 < (Dim+1)), !,
        element(LRD1, LeftRow, LRD1Val),
        Col #=< LRD1Val #<=> ResLD1
    ;
    ResLD1 #= 0),

    LRD2 is Col + Row,
    ((LRD2 > 0, LRD2 < (Dim+1)), !,
        element(LRD2, LeftRow, LRD2Val),
        Col #=< LRD2Val #<=> ResLD2
    ;
    ResLD2 #= 0),

    RRD1 is (Dim+1) - Col + Row,
    ((RRD1 > 0, RRD1 < (Dim+1)), !,
        element(RRD1, RightRow, RRD1Val),
        Col #>= ((Dim+1) - RRD1Val) #<=> ResRD1
    ;
    ResRD1 #= 0),

    RRD2 is Col + Row - (Dim+1),
    ((RRD2 > 0, RRD2 < (Dim+1)), !,
        element(RRD2, RightRow, RRD2Val),
        Col #>= ((Dim+1) - RRD2Val) #<=> ResRD2
    ;
    ResRD2 #= 0),

    TRD1 is Col - Row,
    ((TRD1 > 0, TRD1 < (Dim+1)), !,
        element(TRD1, TopRow, TRD1Val),
        Row #=< TRD1Val #<=> ResTD1
    ;
    ResTD1 #= 0),

    TRD2 is Col + Row,
    ((TRD2 > 0, TRD2 < (Dim+1)), !,
        element(TRD2, TopRow, TRD2Val),
        Row #=< TRD2Val #<=> ResTD2
    ;
    ResTD2 #= 0),

    BRD1 is (4 + 1) - Row + Col,
    ((BRD1 > 0, BRD1 < Dim+1), !,
        element(BRD1, BottomRow, BRD1Val),
        Row #>= ((Dim+1) - BRD1Val) #<=> ResBD1
    ;
    ResBD1 #= 0),

    BRD2 is Row + Col -(Dim+1),
    ((BRD2 > 0 , BRD2 < Dim+1), !,
        element(BRD2, BottomRow, BRD2Val),
        Row #>= ((Dim+1) - BRD2Val) #<=> ResBD2
    ;
    ResBD2 #= 0),

    Val #= ResTR + ResBR + ResRR + ResLR + ResLD1 + ResLD2 + ResRD1 + ResRD2 + ResTD1 + ResTD2 + ResBD1 + ResBD2,

    restrict_board(RestBoard, Dim, TopRow, BottomRow, RightRow, LeftRow).

