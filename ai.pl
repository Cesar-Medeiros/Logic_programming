pickRandomMove(Player, PlayerMove, Dim) :- 	
    allMoves(Player, Dim, List, Len),
    random(0, Len, Random),
    nth0(Random, List, PlayerMove).

allMoves(Player, Dim, List, Len):-
        setof(T, Player^checkMove(Player, T, Dim), List),
        length(List, Len).
    
