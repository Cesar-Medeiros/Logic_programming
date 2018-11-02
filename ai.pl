% :- module(minimax, [minimax/3]).

% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.

minimax(Board, Player, _, Val, 0) :-                     % Pos has no successors
    calculate(Board, Player, Val).

minimax(Board, Player, BestNextBoard, Val, Lvl) :-                     % Pos has successors
    Lvl1 is Lvl - 1,
    findall(NextBoard, (checkMove(Player, PlayCoords, Board, [5,5]), makeMove(Board, Player, PlayCoords, NextBoard, _)), NextBoardList),
    % setof(NextBoard, Player^(checkMove(Player, PlayCoords, Board, [5,5]), makeMove(Board, Player, PlayCoords, NextBoard, _)), NextBoardList),
    forall(member(Elem, NextBoardList), display_game(Elem, [5,5])),
    best(NextBoardList, Player, BestNextBoard, Val, Lvl1), !.




best([Board], Player, Board, Val, Lvl) :-
    nextPlayer(Player, Player2),
    minimax(Board, Player2, _, Val, Lvl), !.

best([Board1 | BoardList], Player, BestBoard, BestVal, Lvl) :-
    nextPlayer(Player, Player2),
    minimax(Board1,Player2, _, Val1, Lvl),
    best(BoardList, Player, Board2, Val2, Lvl),
    betterOf(Player, Board1, Val1, Board2, Val2, BestBoard, BestVal).



betterOf(Player, Board1, Val1, _, Val2, Board1, Val1) :-   % Pos0 better than Pos1
    Player = 0,                         % MIN to move in Pos0
    Val1 > Val2, !                             % MAX prefers the greater value
    ;
    Player = 1,                         % MAX to move in Pos0
    Val1 < Val2, !.                            % MIN prefers the lesser value

betterOf(_,_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0

calculate(Board, Player, 0) :- allMoves(Player, Board, [5,5], _, Len), Val = -Len.


pickRandomMove(Player, PlayerMove, Board, Dim) :- 	
    allMoves(Player, Board, Dim, List, Len),
    random(0, Len, Random),
    nth0(Random, List, PlayerMove).

allMoves(Player, Board, Dim, List, Len):-
        setof(T, Player^checkMove(Player, T, Board, Dim), List),
        length(List, Len).

allMoves(_, _, _, [], 0).

