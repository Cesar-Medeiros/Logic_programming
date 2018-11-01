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
			setSymbol([Rows, 1], 'bAliv'),
			setSymbol([1, Cols], 'rAliv'),
			game(Rows, Cols, 0).

% game(+Board, +Player) - main game cycle.
%   Responsible for making moves and printing the board
game(_, _) :- verifyEnd().
game(Rows, Cols, Player) :- 
			display_game(Rows, Cols, Player),
			printPlayer(Player),
			playInput([Rows, Cols], PlayCoords),
			(checkMove(Player, PlayCoords, [Rows, Cols])
				-> (write('\nOK\n'), makeMove(Player, PlayCoords, PlayerOut))
				; write('\nFail\n')
				),
				retractall(visited(X)),
			game(Rows, Cols, PlayerOut).

% makeMove(+Board, +Player, -BoardOut, -PlayerOut)
%   Responsible for making a move and return the next board and player
makeMove(Player, PlayCoords, PlayerOut) :- 
				(getSymbol(PlayCoords, 'empty')
					-> playerSymbol(Player, Symbol)
					 ; playerSymbolZ(Player, Symbol)),

				setSymbol(PlayCoords, Symbol),
				% PlayerOut is Player.
				nextPlayer(Player, PlayerOut).

checkMove(Player, PlayCoords, Dim) :- 
	getSymbol(PlayCoords, Content),
	(Content = 'empty' ; nextPlayer(Player, PlayerOut), playerSymbol(PlayerOut, Content)),!,
	checkMoveChain(Player, PlayCoords, Dim).


visited([-1, -1]).

checkMoveChain(Player, [Row, Col], [Rows, Cols]) :- 

	not(visited([Row, Col])),
	assertz(visited([Row, Col])),

	Row >= 1, Row =< Rows, Col >=1, Col =< Cols,
	playerSymbol(Player, Symbol),
	playerSymbolZ(Player, SymbolZ),

	(
			(NRow is Row + 1, NCol is Col + 1, (getSymbol([NRow, NCol], Symbol) ; (getSymbol([NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], [Rows, Cols]))));
			(NRow is Row + 0, NCol is Col + 1, (getSymbol([NRow, NCol], Symbol) ; (getSymbol([NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], [Rows, Cols]))));
			(NRow is Row - 1, NCol is Col + 1, (getSymbol([NRow, NCol], Symbol) ; (getSymbol([NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], [Rows, Cols]))));
			(NRow is Row + 1, NCol is Col + 0, (getSymbol([NRow, NCol], Symbol) ; (getSymbol([NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], [Rows, Cols]))));
			(NRow is Row - 1, NCol is Col + 0, (getSymbol([NRow, NCol], Symbol) ; (getSymbol([NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], [Rows, Cols]))));
			(NRow is Row + 1, NCol is Col - 1, (getSymbol([NRow, NCol], Symbol) ; (getSymbol([NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], [Rows, Cols]))));
			(NRow is Row + 0, NCol is Col - 1, (getSymbol([NRow, NCol], Symbol) ; (getSymbol([NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], [Rows, Cols]))));
			(NRow is Row - 1, NCol is Col - 1, (getSymbol([NRow, NCol], Symbol) ; (getSymbol([NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], [Rows, Cols]))))
	).
% verifyEnd() - end game
%  Exit game on user input 'e'
verifyEnd() :- 
			write('Press \'e\' to exit: '),
			read(X), X == 'e'.

playerSymbol(0, 'bAliv').
playerSymbol(1, 'rAliv').
playerSymbolZ(0, 'bDead').
playerSymbolZ(1, 'rDead').


nextPlayer(PlayerIn, PlayerOut) :- PlayerOut is (PlayerIn + 1) mod 2.