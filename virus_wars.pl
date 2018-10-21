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



symbol('empty', ' ').
symbol('rAliv', '◯').
symbol('rDead', '⨂').
symbol('bAliv', '⨉').
symbol('bDead', '●'). 


main :- tab2(Tab), game(Tab, 0).

game(_, _) :- verifyEnd().
game(Tab, Player) :- 
				makeMove(Tab, Player, TabOut, PlayerOut),
				printTab(TabOut),
				game(TabOut, PlayerOut).

makeMove(Tab, Player, TabOut, PlayerOut) :- 
				TabOut = Tab, 
				PlayerOut is (Player + 1) mod 2.

verifyEnd() :- get_char(X), X == 'e'.




/*
Print board functions
*/

printTab(Tab) :- length(Tab, N), printTab_Begin(Tab, N).

printTab_Begin([L | T], N) :- 
			printChar(' ', 4),
			printFirstLine_Begin(L),
			number_string(N, S),
			writef('%3r ', [S]),
			N1 is N - 1,
			printLine(L), 
			printTab_Middle(T , N1).


printTab_Middle([L | T], N) :- 
			printChar(' ', 4),
			printSeparationLine(L),
			number_string(N, S),
			writef('%3r ', [S]),
			N1 is N - 1,
			printLine(L),
			printTab_Middle(T, N1).


printTab_Middle([], _) :- 
			printTab_End().


printTab_End() :- 		
			printChar(' ', 4), 
			tab2([L | _]), 
			printFinalLine(L),
			printChar(' ', 4),
			printCoordsLine(L).



printLine([C | L]) :- printLine_Begin([C | L]).

printLine_Begin([C | L])  :-  
				put_code(9475),
				put_char(' '),
				printCell(C),
				put_char(' '),
				printLine_Middle(L).

printLine_Middle([C | L]) :- 
				put_code(9474),
				put_char(' '),
				printCell(C),
				put_char(' '),
				printLine_Middle(L).

printLine_Middle([]):- 		printLine_End().
printLine_End() :- 		put_code(9475), nl.


printCell(C) :- 		symbol(C,V), put_code(V).


/*
Decoration Functions
*/

printFirstLine(L) :- 		printFirstLine_Begin(L).

printFirstLine_Begin([_ | L])  :- 
				printChar('┏', 1),
				printChar('━', 3),
				printFirstLine_Middle(L).

printFirstLine_Middle([_ | L]) :- 
				printChar('┯', 1),
				printChar('━', 3),
				printFirstLine_Middle(L).

printFirstLine_Middle([]) :- 	printFirstLine_End().

printFirstLine_End() :- 	printChar('┓', 1), nl.



printSeparationLine(L) :- printSeparationLine_Begin(L).

printSeparationLine_Begin([_ | L]) :- 
				printChar('┠', 1),
				printChar('─', 3),
				printSeparationLine_Middle(L).

printSeparationLine_Middle([_ | L]) :-
				printChar('┼', 1),
				printChar('─', 3),
				printSeparationLine_Middle(L).

printSeparationLine_Middle([]) :- printSeparationLine_End().
printSeparationLine_End() :- printChar('┨', 1), nl.


printFinalLine(L) :- printFinalLine_Begin(L).
printFinalLine_Begin([_ | L])  :- 
				printChar('┗', 1),
				printChar('━', 3),
				printFinalLine_Middle(L).

printFinalLine_Middle([_ | L]) :- 
				printChar('┷', 1),
				printChar('━', 3),
				printFinalLine_Middle(L).

printFinalLine_Middle([]) :- printFinalLine_End().

printFinalLine_End() :- printChar('┛', 1), nl.


printCoordsLine(L) :- printCoordsLine_Aux(L, 97).

printCoordsLine_Aux([_ | L], N) :- 
				printChar(' ', 2),
				put_code(N),
				printChar(' ', 1),
				N1 is N + 1,
				printCoordsLine_Aux(L, N1).

printCoordsLine_Aux([], _) :- nl.




printChar(_, 0).
printChar(C, N) :- 
				put_char(C),
				N1 is N - 1,
				printChar(C, N1).

