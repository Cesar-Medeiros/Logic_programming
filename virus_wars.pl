:- consult('display.pl').
:- consult('board.pl').
:- consult('input.pl').
:- consult('menu.pl').
:- consult('ai.pl').


main(Rows, Cols) :- 
			mainMenu,
			mainMenuInput(GameType),
			getPlayersType(GameType, PlayersType),
			Dim = [Rows, Cols],
			createBoard(Board, Dim),
			game(0, PlayersType, Board, Dim).


getPlayersType(1, ['user', 'user']).
getPlayersType(2, ['user', 'computer']).
getPlayersType(3, ['computer', 'computer']).


game(Player, _, Board, Dim) :- gameOver(Player, Board, Dim).

game(Player, PlayersType, Board, Dim) :- 
			cls,
			printPlayer(Player),
			display_game(Board, Dim),
			
			nth0(Player, PlayersType, PlayerType),

			move(Player, PlayerType, Board, Dim, BoardOut, PlayerOut),
			

			game(PlayerOut, PlayersType, BoardOut, Dim).



move(Player, 'user', Board, Dim, BoardOut, PlayerOut):-
		playInput(Dim, PlayerMove), !,
		(checkMove(Player, PlayerMove, Board, Dim)
			-> (/*write('\nValid Move\n'), write(PlayerMove),nl, */makeMove(Board, Player, PlayerMove, BoardOut, PlayerOut))
			 ; write('\nInvalid Move\n'), BoardOut = Board, PlayerOut is Player).

move(Player, 'computer', Board, Dim, BoardOut, PlayerOut):-
		% pickRandomMove(Player, PlayerMove, Board, Dim).
		minimax(Board, Player, BoardOut, _, 2),
		nextPlayer(Player, PlayerOut).
		 


gameOver(Player, Board, Dim) :-
		allMoves(Player, Board, Dim, _List, Len),
		% write('Possible moves: '),
		% write(List),
		% nl,
		!,
		Len = 0,
		nextPlayer(Player, PreviousPlayer),
		getPlayerSymbol(PreviousPlayer, Symbol),
		format('Player ~w won.~n', [Symbol]).


% makeMove(+Board, +Player, -BoardOut, -PlayerOut)
%   Responsible for making a move and return the next board and player
makeMove(Board, Player, PlayCoords, BoardOut, PlayerOut) :- 
				(getSymbol(Board, PlayCoords, 'empty')
					-> playerValue(Player, Symbol)
					 ; playerValueZ(Player, Symbol)),

				setSymbol(Board, PlayCoords, Symbol, BoardOut),
				nextPlayer(Player, PlayerOut).

checkMove(Player, PlayCoords, Board, Dim) :-
	getSymbol(Board, PlayCoords, Content),
	(Content = 'empty' ; nextPlayer(Player, PlayerOut), playerValue(PlayerOut, Content)),
	retractall(visited(_)),
	checkMoveChain(Player, PlayCoords, Board, Dim).


checkMoveChain(Player, [Row, Col], Board, [Rows, Cols]) :- 
	not(visited([Row, Col])),
	assertz(visited([Row, Col])),

	Row >= 1, Row =< Rows, Col >=1, Col =< Cols,
	playerValue(Player, Symbol),
	playerValueZ(Player, SymbolZ),

	(
			(NRow is Row + 1, NCol is Col + 1, (getSymbol(Board, [NRow, NCol], Symbol) ; (getSymbol(Board, [NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], Board, [Rows, Cols]))));
			(NRow is Row + 0, NCol is Col + 1, (getSymbol(Board, [NRow, NCol], Symbol) ; (getSymbol(Board, [NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], Board, [Rows, Cols]))));
			(NRow is Row - 1, NCol is Col + 1, (getSymbol(Board, [NRow, NCol], Symbol) ; (getSymbol(Board, [NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], Board, [Rows, Cols]))));
			(NRow is Row + 1, NCol is Col + 0, (getSymbol(Board, [NRow, NCol], Symbol) ; (getSymbol(Board, [NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], Board, [Rows, Cols]))));
			(NRow is Row - 1, NCol is Col + 0, (getSymbol(Board, [NRow, NCol], Symbol) ; (getSymbol(Board, [NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], Board, [Rows, Cols]))));
			(NRow is Row + 1, NCol is Col - 1, (getSymbol(Board, [NRow, NCol], Symbol) ; (getSymbol(Board, [NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], Board, [Rows, Cols]))));
			(NRow is Row + 0, NCol is Col - 1, (getSymbol(Board, [NRow, NCol], Symbol) ; (getSymbol(Board, [NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], Board, [Rows, Cols]))));
			(NRow is Row - 1, NCol is Col - 1, (getSymbol(Board, [NRow, NCol], Symbol) ; (getSymbol(Board, [NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], Board, [Rows, Cols]))))
	).

playerValue(0, 'bAliv').
playerValue(1, 'rAliv').
playerValueZ(0, 'bDead').
playerValueZ(1, 'rDead').

nextPlayer(PlayerIn, PlayerOut) :- PlayerOut is (PlayerIn + 1) mod 2.