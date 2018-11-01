:- consult('display.pl').
:- consult('board.pl').
:- consult('input.pl').
:- consult('menu.pl').

% main - succeeds when game ends;
%   board "global variable" must have empty board
main(Rows, Cols) :- 
			mainMenu,
			mainMenuInput(_X),
			createBoard(Rows, Cols),
			game(Rows, Cols, 0).

% game(+Board, +Player) - main game cycle.
%   Responsible for making moves and printing the board
game(_, _) :- verifyEnd().
game(Rows, Cols, Player) :- 
			display_game(Rows, Cols, Player),
			playInput([Rows, Cols], PlayCoords),
			makeMove(Player, PlayCoords, PlayerOut),
			game(Rows, Cols, PlayerOut).

% makeMove(+Board, +Player, -BoardOut, -PlayerOut)
%   Responsible for making a move and return the next board and player
makeMove(Player, [Row, Col], PlayerOut) :- 
				playerSymbol(Player, Symbol),
				setSymbol(Row, Col, Symbol),
				PlayerOut is (Player + 1) mod 2.

% verifyEnd() - end game
%  Exit game on user input 'e'
verifyEnd() :- 
			write('Press \'e\' to exit: '),
			read(X), X == 'e'.

playerSymbol(0, 'bAliv').
playerSymbol(1, 'rAliv').  