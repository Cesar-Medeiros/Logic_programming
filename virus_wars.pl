tab1([
	['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
	['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
	['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
	['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
	['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
	['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
	['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
	['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
	['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
	['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty']
    ]).


tab2([
	['empty', 'empty', 'empty', 'bDead', 'rDead', 'bDead', 'bAliv', 'bAliv', 'bDead', 'bDead'],
	['empty', 'empty', 'empty', 'empty', 'rDead', 'bDead', 'bDead', 'empty', 'empty', 'rDead'],
	['empty', 'empty', 'empty', 'bDead', 'rDead', 'bDead', 'rDead', 'bDead', 'bDead', 'bDead'],
	['empty', 'empty', 'empty', 'bDead', 'rDead', 'bDead', 'bDead', 'bDead', 'bDead', 'bDead'],
	['empty', 'empty', 'empty', 'rDead', 'rDead', 'bDead', 'bDead', 'rDead', 'empty', 'empty'],
	['empty', 'empty', 'empty', 'rDead', 'rDead', 'bDead', 'bDead', 'rDead', 'empty', 'rDead'],
	['empty', 'rAliv', 'bDead', 'rDead', 'rDead', 'bDead', 'rDead', 'empty', 'empty', 'empty'],
	['empty', 'rAliv', 'rDead', 'rDead', 'empty', 'bDead', 'empty', 'empty', 'empty', 'empty'],
	['rDead', 'rDead', 'rDead', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
	['rDead', 'rDead', 'rDead', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty']
    ]).



symbol('empty',' ').
symbol('rAliv', '◯'). %O
symbol('rDead', '⨂'). %E
symbol('bAliv', '⨉'). %X
symbol('bDead', '●'). %.


main :- tab2(Tab), game(Tab, 0).

%game(Tab, _) :- endGame(Tab, N). 

game(Tab, Player) :- makeMove(Tab, Player, TabOut, PlayerOut), printTab(TabOut), verifyEnd(), game(TabOut, PlayerOut).

makeMove(Tab, Player, TabOut, PlayerOut) :- TabOut = Tab, PlayerOut is (Player + 1) mod 2.

verifyEnd() :- get_char(X), X \= 'e'.

/*
Print board functions
*/

printTab(Tab) :- printTab_Begin(Tab).
printTab_Begin([L | T]) :- printFirstLine_Begin(L), printLine(L), printTab_Middle(T).
printTab_Middle([L | T]) :- printSeparationLine(L), printLine(L), printTab_Middle(T).
printTab_Middle([]) :- printTab_End().
printTab_End() :- tab2([L | _]), printFinalLine(L).



printLine([C | L]) :- printLine_Begin([C | L]).
printLine_Begin([C | L])  :- put_code(9475), put_char(' '), printCell(C), put_char(' '), printLine_Middle(L).
printLine_Middle([C | L]) :- put_code(9474), put_char(' '), printCell(C), put_char(' '), printLine_Middle(L).
printLine_Middle([]):- printLine_End().
printLine_End() :- put_code(9475), nl.



printCell(C) :- symbol(C,V), put_code(V).



/*
Decoration Functions
*/

printFirstLine(L) :- printFirstLine_Begin(L).
printFirstLine_Begin([_ | L])  :- put_code(9487), put_code(9473), put_code(9473), put_code(9473), printFirstLine_Middle(L).
printFirstLine_Middle([_ | L]) :- put_code(9519), put_code(9473), put_code(9473), put_code(9473), printFirstLine_Middle(L).
printFirstLine_Middle([]) :- printFirstLine_End().
printFirstLine_End() :- put_code(9491), nl.


printSeparationLine(L) :- printSeparationLine_Begin(L).
printSeparationLine_Begin([_ | L]) :- put_code(9504), put_code(9472), put_code(9472), put_code(9472), printSeparationLine_Middle(L).
printSeparationLine_Middle([_ | L]) :- put_code(9532), put_code(9472), put_code(9472), put_code(9472), printSeparationLine_Middle(L).
printSeparationLine_Middle([]) :- printSeparationLine_End().
printSeparationLine_End() :- put_code(9512), nl.


printFinalLine(L) :- printFinalLine_Begin(L).
printFinalLine_Begin([_ | L])  :- put_code(9495), put_code(9473), put_code(9473), put_code(9473), printFinalLine_Middle(L).
printFinalLine_Middle([_ | L]) :- put_code(9527), put_code(9473), put_code(9473), put_code(9473), printFinalLine_Middle(L).
printFinalLine_Middle([]) :- printFinalLine_End().
printFinalLine_End() :- put_code(9499), nl.

