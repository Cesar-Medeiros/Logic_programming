:-use_module(library(clpfd)).
:-use_module(library(lists)).

% getBoard([[4, 6, 6, 5],[5, 2, 6, 4],[4, 6, 6, 6],[4, 6, 6, 4]]).


getBoard([
        cell(1, 1, 2),
        cell(1, 2, 1),
        cell(1, 3, 1),
        cell(1, 4, 2),

        cell(2, 1, 1),
        cell(2, 2, 0),
        cell(2, 3, 0),
        cell(2, 4, 1),

        cell(3, 1, 1),
        cell(3, 2, 0),
        cell(3, 3, 0),
        cell(3, 4, 1),

        cell(4, 1, 2),
        cell(4, 2, 1),
        cell(4, 3, 1),
        cell(4, 4, 2)
    ]).

solve:-
    length(TopRow, 4),      domain(TopRow, 0, 4),
    length(RightRow, 4),    domain(RightRow, 0, 4),
    length(BottomRow, 4),   domain(BottomRow, 0, 4),
    length(LeftRow, 4),     domain(LeftRow, 0, 4),

    getBoard(Board),

    restrict_board(Board, TopRow, BottomRow, RightRow, LeftRow),

    append([TopRow, BottomRow, RightRow, LeftRow], List),

    labeling([], List),
    write(List),
    write('\n'),
    fail.


restrict_board([], TopRow, BottomRow, RightRow, LeftRow).

restrict_board([Cell | RestBoard], TopRow, BottomRow, RightRow, LeftRow) :-

    Cell = cell(Row, Col, Val),

    element(Col, TopRow,    TRVal),
    element(Col, BottomRow, BRVal),

    element(Row, RightRow,  RRVal),
    element(Row, LeftRow,   LRVal),

    Row #=< TRVal #<=> ResTR,
    Row #>= (4 + 1 - BRVal) #<=> ResBR,

    Col #=< LRVal #<=> ResLR,
    Col #>= (4 + 1 - RRVal) #<=> ResRR,



    LRD1 in 1..4,
    LRD1 #= -Col + Row,
    element(LRD1, LeftRow, LRD1Val),
    Col #=< LRD1Val,

    LRD2 in 1..4,
    LRD2 #= Col + Row,
    element(LRD2, LeftRow,  LRD2Val),
    Col #=< LRD2Val,


    RRD1 in 1..4,
    RRD1 #= 4 + 1 - Col + Row,
    element(RRD1, RightRow, RRD1Val),
    Col #>= (4 + 1 - RRD1Val),

    RRD2 in 1..4,
    RRD2 #= - (4 + 1) + Col + Row,
    element(RRD2, RightRow, RRD2Val),
    Col #>= (4 + 1 - RRD2Val),



    TRD1 in 1..4,
    TRD1 #= - Row + Col,
    element(TRD1, TopRow, TRD1Val),
    Row #=< TRD1Val,

    TRD2 in 1..4,
    TRD2 #= Col + Row,
    element(TRD2, TopRow, TRD2Val),
    Row #=< TRD2Val,


    BRD1 in 1..4,
    BRD1 #= (N + 1) - Row + Col,
    element(BRD1, BottomRow, BRD1Val),
    Row #>= (4 + 1 - BRD1Val),

    BRD2 in 1..4,
    BRD2 #= -(N + 1) + Row + Col,
    element(BRD2, BottomRow, BRD2Val),
    Row #>= (4 + 1 - BRD2Val),


    Val #= ResTR + ResBR + ResRR + ResLR + LRD1 + LRD2 + RRD1 + RRD2 + TRD1 + TRD2 + BRD1 + BRD2,

    restrict_board(RestBoard, TopRow, BottomRow, RightRow, LeftRow).

