% board(+Board)
%   Select a Board to start the game with
createBoard(Board, [Rows, Cols]) :-
            findall(cell(R, C, 'empty'), (between(1, Rows, R), between(1, Cols, C)),  Board).

getSymbol(BoardCell-_Dim, [Row, Col], Value) :- 
            member(cell(Row, Col, Value), BoardCell).

setSymbol(BoardCell-Dim, [Row, Col], Value, BoardOut-Dim) :- 
            select(cell(Row,Col,_), BoardCell, BoardRem), 
            append(BoardRem, [cell(Row, Col, Value)], BoardOut).

isPositionValid(_BoardCell-[Rows, Cols], [Row, Col]) :-
    Row >= 1, Row =< Rows, Col >=1, Col =< Cols.
