% :- module(minimax, [minimax/3]).

% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.


% minimax(Board, Player, _, Val, 0) :-                     % Pos has no successors
%     value(Board, Player, Val).

% minimax(Board, Player, BestNextBoard, Val, Lvl) :-                     % Pos has successors
%     Lvl1 is Lvl - 1,
%     generateValidMoves(Player, List, Board),
%     findall(NextBoard, (member(PlayCoords, List), makeMove(Board, Player, PlayCoords, NextBoard, _)), NextBoardList),
%     best(NextBoardList, Player, BestNextBoard, ValAux, Lvl1),
%     length(List, Len),
%     Val is ValAux + Len,
%     !.


% minimax(Board, Player, _, Val, _) :-                     % Pos has no successors
%     value(Board, Player, Val).



% best([Board], Player, Board, Val, Lvl) :-
%     nextPlayer(Player, Player2),
%     minimax(Board, Player2, _, Val, Lvl), !.

% best([Board1 | BoardList], Player, BestBoard, BestVal, Lvl) :-
%     nextPlayer(Player, Player2),
%     minimax(Board1,Player2, _, Val1, Lvl),
%     best(BoardList, Player, Board2, Val2, Lvl),
%     betterOf(Player2, Board1, Val1, Board2, Val2, BestBoard, BestVal).



% betterOf(Player, Board1, Val1, _, Val2, Board1, Val1) :-   % Board1 better than Board2
%     Player = 1,                         % MIN to move in Pos0
%     Val1 > Val2, !                             % MAX prefers the greater value
%     ;
%     Player = 0,                         % MAX to move in Pos0
%     Val1 < Val2, !.                            % MIN prefers the lesser value

% betterOf(_Player,_Board1, _Val1, Board2, Val2, Board2, Val2).        % Otherwise Pos1 better than Pos0

value(_, 0, 100).
value(_, 1, -100).


ai(Board, Player, Lvl, Move) :-
    minimax(Board, Player, Player, Move, _Val, Lvl).

minimax(Board, Player, _, _, Val, 0) :-
    value(Board, Player, Val), !.

minimax(Board, Player, MaxPlayer, BestNextMove, Val, Lvl) :-
    Lvl1 is Lvl - 1,
    valid_moves(Board, Player, MovesList),
    best(Board, MovesList, Player, MaxPlayer, BestNextMove, ValAux, Lvl1),
    length(MovesList, Len),
    Val is ValAux + Len,
    !.

minimax(Board, Player, _, _, Val, _) :-
    value(Board, Player, Val), !.

best(Board, MovesList, Player, MaxPlayer, BestNextMove, BestVal, Lvl) :-
    setof(
        Val-Move,
        (   
            member(Move, MovesList),
            makeMove(Board, Player, Move, NewBoard, NewPlayer),
            minimax(NewBoard, NewPlayer, MaxPlayer, BestNextMove, Val, Lvl)
        ), 
        ScoreList
        ),
        
    length(ScoreList, Len),
    
    (Player = MaxPlayer, !, 
        nth0(0, ScoreList, BestVal-BestNextMove)
        ; 
        Last is Len - 1, nth0(Last, ScoreList, BestVal-BestNextMove)).