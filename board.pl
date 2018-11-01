% board(+Board)
%   Select a Board to start the game with
createBoard(Rows, Cols) :-
    forall(( between(1, Rows, R),
             between(1, Cols, C)
           ),
           assertz(game_board(R, C, 'empty'))).


getSymbol(Row, Col, Content) :- game_board(Row, Col, Content).

setSymbol(Row, Col, Content) :-  retract(game_board(Row, Col, _)), assertz(game_board(Row, Col, Content)).