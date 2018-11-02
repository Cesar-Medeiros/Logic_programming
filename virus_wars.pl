:- consult('display.pl').
:- consult('board.pl').
:- consult('input.pl').
:- consult('menu.pl').
:- consult('ai.pl').




main(Rows, Cols) :- 
			% mainMenu,
			% mainMenuInput(_X),
			Dim = [Rows, Cols],
			createBoard(Dim),
			game(0, Dim).



game(Player, Dim) :- gameOver(Player, Dim).

game(Player, Dim) :- 
			display_game(Dim),
			printPlayer(Player),
			
			
			(play(Player, PlayerMove, Dim)
				-> (write('\nValid Move\n'), write(PlayerMove),nl, makeMove(Player, PlayerMove, PlayerOut))
				 ; write('\nInvalid Move\n'), PlayerOut is Player),

			game(PlayerOut, Dim).

play(Player, PlayerMove, Dim):-
		Player = 0,
		playInput(Dim, PlayerMove), !,
		checkMove(Player, PlayerMove, Dim).

play(Player, PlayerMove, Dim) :- 
		Player = 1,
		pickRandomMove(Player, PlayerMove, Dim).


gameOver(Player, Dim) :-
		allMoves(Player, Dim, List, Len),
		write('Possible moves: '),
		write(List),
		nl,
		!,
		Len = 0.

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
	(Content = 'empty' ; nextPlayer(Player, PlayerOut), playerSymbol(PlayerOut, Content)),
	retractall(visited(X)),
	checkMoveChain(Player, PlayCoords, Dim).


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

playerSymbol(0, 'bAliv').
playerSymbol(1, 'rAliv').
playerSymbolZ(0, 'bDead').
playerSymbolZ(1, 'rDead').


nextPlayer(PlayerIn, PlayerOut) :- PlayerOut is (PlayerIn + 1) mod 2.