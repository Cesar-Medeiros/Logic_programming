% board(+Board)
%   Select a Board to start the game with
createBoard(Board, [Rows, Cols]) :-
            findall(cell(R, C, 'empty'), (between(1, Rows, R), between(1, Cols, C)),  Board1),
            setSymbol(Board1, [Rows, 1], 'bAliv',Board2),
            setSymbol(Board2, [1, Cols], 'rAliv', Board3),
            Board = Board3.

getSymbol(Board, [Row, Col], Value) :- 
            member(cell(Row, Col, Value), Board).

setSymbol(Board, [Row, Col], Value, BoardOut) :- 
            select(cell(Row,Col,_), Board, BoardRem), 
            append(BoardRem, [cell(Row, Col, Value)], BoardOut).
