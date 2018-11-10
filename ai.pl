% :- module(minimax, [minimax/3]).

% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.


minimax(Board, Player, _, Val, 0) :-                     % Pos has no successors
    calculate(Board, Player, Val).
    % write('Calculate: '),
    % write(Val),
    % nl.

minimax(Board, Player, BestNextBoard, Val, Lvl) :-                     % Pos has successors
    % printPlayer(Player),
    % write('Board-: '),
    % nl,
    % forall(between(1, 1, _), display_game(Board, [3,3])),
    Lvl1 is Lvl - 1,
    generateValidMoves(Player, List, Board),
    findall(NextBoard, (member(PlayCoords, List), makeMove(Board, Player, PlayCoords, NextBoard, _)), NextBoardList),
    % write('Possible moves: '),
    % nl,
    % forall(member(Elem, NextBoardList), display_game(Elem, [3,3])),
    % nl,
    % read(_),
    best(NextBoardList, Player, BestNextBoard, ValAux, Lvl1),
    length(List, Len),
    Val is ValAux + Len,
    % write('Best Val: '), write(Val),
    !.


minimax(Board, Player, _, Val, _) :-                     % Pos has no successors
    calculate(Board, Player, Val).





best([Board], Player, Board, Val, Lvl) :-
    nextPlayer(Player, Player2),
    minimax(Board, Player2, _, Val, Lvl), !.

best([Board1 | BoardList], Player, BestBoard, BestVal, Lvl) :-
    nextPlayer(Player, Player2),
    minimax(Board1,Player2, _, Val1, Lvl),
    best(BoardList, Player, Board2, Val2, Lvl),
    betterOf(Player2, Board1, Val1, Board2, Val2, BestBoard, BestVal).



betterOf(Player, Board1, Val1, _, Val2, Board1, Val1) :-   % Board1 better than Board2
    Player = 1,                         % MIN to move in Pos0
    Val1 > Val2, !                             % MAX prefers the greater value
    ;
    Player = 0,                         % MAX to move in Pos0
    Val1 < Val2, !.                            % MIN prefers the lesser value

betterOf(_Player,_Board1, _Val1, Board2, Val2, Board2, Val2).        % Otherwise Pos1 better than Pos0

calculate(_, 0, 100).
calculate(_, 1, -100).


pickRandomMove(Player, PlayerMove, Board, Dim) :- 	
    allMoves(Player, Board, Dim, List, Len),
    random(0, Len, Random),
    nth0(Random, List, PlayerMove). 

allMoves(Player, Board, Dim, List, Len):-
        generateValidMoves(Player, List, Board, Dim),
        length(List, Len).

allMoves(_, _, _, [], 0).

