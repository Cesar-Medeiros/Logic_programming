ai(Board, Player, Lvl, Move) :-
    minimax(Board, Player, Player, Move, _Val, Lvl),
    write(Move).

minimax(Board, Player, _, _, Val, 0) :-
    write('Test1'),nl,
    value(Board, Player, Val), 
    write('EXIT'),!.

minimax(Board, Player, MaxPlayer, BestNextMove, Val, Lvl) :-
    Lvl1 is Lvl - 1,
    valid_moves(Board, Player, MovesList),
    best(Board, MovesList, Player, MaxPlayer, BestNextMove, Val, Lvl1),
    !.

minimax(_, Player, MaxPlayer, _, Val, _) :-
    (Player = MaxPlayer, !, Val = -100; Val = 100).

best(Board, MovesList, Player, MaxPlayer, BestNextMove, BestVal, Lvl) :-
    setof(
        Val-Move,
        (   
            member(Move, MovesList),
            makeMove(Board, Player, Move, NewBoard),
            nextPlayer(Player, NewPlayer, Turn, NewTurn),
            minimax(NewBoard, NewPlayer, MaxPlayer, _, Val, Lvl)
        ), 
        ScoreList
        ),
    
    (Player = MaxPlayer, !, 
        Index is 0
        ; 
        length(ScoreList, ScoreListLen),
        Index is ScoreListLen - 1
    ),

    nth0(Index, ScoreList, BestVal-_),
    findall(Move, member(BestVal-Move, ScoreList), BestMoves),
    length(BestMoves, BestMovesLen),
    random(0, BestMovesLen, N),
    nth0(N, BestMoves, BestNextMove).


value(Board, Player, Val) :- 
    valid_moves(Board, Player, MovesList), 
    length(MovesList, L), Val = L. 