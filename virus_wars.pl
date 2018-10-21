board_begin([
	['empty','empty','empty','empty','empty','empty','empty','empty','empty','empty'],
	['empty','empty','empty','empty','empty','empty','empty','empty','empty','empty'],
	['empty','empty','empty','empty','empty','empty','empty','empty','empty','empty'],
	['empty','empty','empty','empty','empty','empty','empty','empty','empty','empty'],
	['empty','empty','empty','empty','empty','empty','empty','empty','empty','empty'],
	['empty','empty','empty','empty','empty','empty','empty','empty','empty','empty'],
	['empty','empty','empty','empty','empty','empty','empty','empty','empty','empty'],
	['empty','empty','empty','empty','empty','empty','empty','empty','empty','empty'],
	['empty','empty','empty','empty','empty','empty','empty','empty','empty','empty'],
	['empty','empty','empty','empty','empty','empty','empty','empty','empty','empty']
    ]).


board_play([ 
	['empty','empty','empty','empty','empty','empty','empty','empty','bDead','bDead'],
	['bAliv','bAliv','empty','empty','empty','rAliv','rAliv','rAliv','rDead','bDead'],
	['empty','bAliv','empty','empty','bDead','bDead','bDead','bDead','rAliv','bDead'],
	['empty','bAliv','empty','bAliv','bDead','bDead','bDead','bDead','bDead','rAliv'],
	['empty','bAliv','bAliv','bAliv','rDead','bDead','bDead','rAliv','rAliv','rAliv'],
	['empty','empty','empty','bAliv','empty','empty','bDead','bDead','empty','rAliv'],
	['empty','empty','empty','empty','bDead','bDead','bDead','empty','empty','rAliv'],
	['empty','empty','empty','empty','empty','empty','empty','bAliv','empty','bDead'],
	['empty','empty','empty','empty','empty','empty','empty','empty','bDead','bDead'],
	['empty','empty','empty','empty','empty','empty','empty','empty','empty','empty'] 
	]).

board_final([ 
	['empty','empty','empty','empty','empty','empty','empty','empty','bDead','bDead'],
	['bAliv','bAliv','empty','empty','empty','bDead','bDead','bDead','rDead','bDead'],
	['empty','bAliv','empty','empty','bDead','bDead','bDead','bDead','bDead','bDead'],
	['empty','bAliv','empty','bAliv','bDead','bDead','bDead','bDead','bDead','rAliv'],
	['empty','bAliv','bAliv','bAliv','rDead','bDead','bDead','rAliv','rAliv','rAliv'],
	['empty','empty','empty','bAliv','empty','empty','bDead','bDead','bDead','rAliv'],
	['empty','empty','empty','empty','bDead','bDead','bDead','bDead','bDead','bDead'],
	['empty','empty','empty','bAliv','rDead','bDead','bDead','rDead','empty','bDead'],
	['empty','empty','empty','empty','bDead','bDead','bDead','empty','bDead','bDead'],
	['empty','empty','empty','empty','empty','empty','empty','empty','empty','empty'] 
	]).

% board(+Board)
%   Select a Board to start the game with
board(Board) :- board_play(Board).

% main - succeeds when game ends;
%   board "global variable" must have empty board
main :- board(Board), game(Board, 0).


% game(+Board, +Player) - main game cycle.
%   Responsible for making moves and printing the board
game(_, _) :- verifyEnd().
game(Board, Player) :- 
				makeMove(Board, Player, BoardOut, PlayerOut),
				display_game(BoardOut, PlayerOut),
				game(BoardOut, PlayerOut).

% makeMove(+Board, +Player, -BoardOut, -PlayerOut)
%   Responsible for making a move and return the next board and player
makeMove(Board, Player, BoardOut, PlayerOut) :- 
				BoardOut = Board, 
				PlayerOut is (Player + 1) mod 2.

% verifyEnd() - end game
%  Exit game on user input 'e'
verifyEnd() :- write('Press \'e\' to exit: '), get_char(X), X == 'e'.


% display_game(+Board)
%   Responsible for printing the board
display_game(Board,_Player) :- 
			length(Board, N), 
			printBoard_Begin(Board, N).

% printBoard_Begin(+Board, +RowNumber)
%   Aux function to print first line formated
printBoard_Begin([L | T], N) :- 
			printChar(' ', 4),
			printFirstLine_Begin(L),
			printBoard_Middle([L | T] , N).


% printBoard_Middle(+Board, +RowNumber)
%   Aux function to print Board content
printBoard_Middle([L | []], N) :- printBoard_End(L, N).

printBoard_Middle([L | T], N) :- 
			number_string(N, S),
			writef('%3r ', [S]),
			printLine(L),
			printChar(' ', 4),
			printSeparationLine(L),
			N1 is N - 1,
			printBoard_Middle(T, N1).

% printBoard_End(+Board, +RowNumber)
%   Aux function to print last line formated and coordinates
printBoard_End(L, N) :- 		
			number_string(N, S),
			writef('%3r ', [S]),
			printLine(L),
			printChar(' ', 4), 
			printFinalLine(L),
			printChar(' ', 4),
			printCoordsLine(L).



% printLine(+Line)
%   Print a line decorated
printLine([C | L]) :- printLine_Begin([C | L]).

printLine_Begin([C | L])  :-  
				put_code('┃'),
				put_char(' '),
				printCell(C),
				put_char(' '),
				printLine_Middle(L).

printLine_Middle([C | L]) :- 
				put_code('│'),
				put_char(' '),
				printCell(C),
				put_char(' '),
				printLine_Middle(L).

printLine_Middle([]):- printLine_End().
printLine_End() :- put_code('┃'), nl.


% printLine(+Character)
%   Translate cell content to a symbol and prints it
printCell(C) :- 
				symbol(C,V), 
				put_code(V).


% printFirstLine(+Line)
%   Prints first decorative line
printFirstLine(L) :- printFirstLine_Begin(L).

printFirstLine_Begin([_ | L])  :- 
				printChar('┏', 1),
				printChar('━', 3),
				printFirstLine_Middle(L).

printFirstLine_Middle([_ | L]) :- 
				printChar('┯', 1),
				printChar('━', 3),
				printFirstLine_Middle(L).

printFirstLine_Middle([]) :- printFirstLine_End().

printFirstLine_End() :- printChar('┓', 1), nl.


% printSeparationLine(+Line)
%   Prints separation decorative line
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



% printFinalLine(+Line)
%   Prints final decorative line
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


% printCoordsLine(+Line)
%   Prints column coordenates line
printCoordsLine(L) :- printCoordsLine_Aux(L, 97).
printCoordsLine_Aux([_ | L], N) :- 
				printChar(' ', 2),
				put_code(N),
				printChar(' ', 1),
				N1 is N + 1,
				printCoordsLine_Aux(L, N1).

printCoordsLine_Aux([], _) :- nl.



% printChar(+Char, +N)
%   Print a Char N times
printChar(_, 0).
printChar(C, N) :- 
				put_char(C),
				N1 is N - 1,
				printChar(C, N1).



% symbol(+String, -Symbol)
%   Translate internal representation to a symbol
symbol('empty', ' ').
symbol('rAliv', '◯').
symbol('rDead', '⨂').
symbol('bAliv', '⨉').
symbol('bDead', '●'). 