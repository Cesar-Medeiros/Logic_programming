:- consult('display.pl').
:- consult('board.pl').
:- consult('input.pl').
:- consult('menu.pl').

% main - succeeds when game ends;
%   board "global variable" must have empty board
main(Rows, Cols) :- 
			mainMenu,
			mainMenuInput(X),
			createBoard(Board, Rows, Cols),
			game(Board, 0).

% game(+Board, +Player) - main game cycle.
%   Responsible for making moves and printing the board
game(_, _) :- verifyEnd().
game(Board, Player) :- 
			display_game(Board, Player),
			playInput([3, 3], PlayerCoords),
			makeMove(Board, Player, BoardOut, PlayerOut),
			game(BoardOut, PlayerOut).

% makeMove(+Board, +Player, -BoardOut, -PlayerOut)
%   Responsible for making a move and return the next board and player
makeMove(Board, Player, BoardOut, PlayerOut) :- 
				BoardOut = Board, 
				PlayerOut is (Player + 1) mod 2.

% verifyEnd() - end game
%  Exit game on user input 'e'
verifyEnd() :- 
			write('Press \'e\' to exit: '),
			read(X), X == 'e'.