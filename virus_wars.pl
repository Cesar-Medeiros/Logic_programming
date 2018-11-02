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
			createBoard(Dim),
			game(0, PlayersType, Dim),
			clearBoard.


getPlayersType(1, ['user', 'user']).
getPlayersType(2, ['user', 'computer']).
getPlayersType(3, ['computer', 'computer']).
getPlayersType(GameType, PlayersType) :- write('Option not working for now'), fail.


game(Player, _, Dim) :- gameOver(Player, Dim).

game(Player, PlayersType, Dim) :- 
			cls,
			printPlayer(Player),
			display_game(Dim),
			
			nth0(Player, PlayersType, PlayerType),

			(getMove(Player, PlayerType, Dim, PlayerMove)
				-> (/*write('\nValid Move\n'), write(PlayerMove),nl, */makeMove(Player, PlayerMove, PlayerOut))
				 ; write('\nInvalid Move\n'), PlayerOut is Player),

			game(PlayerOut, PlayersType, Dim).

getMove(Player, 'user', Dim, PlayerMove):-
		playInput(Dim, PlayerMove), !,
		checkMove(Player, PlayerMove, Dim).

getMove(Player, 'computer', Dim, PlayerMove) :-
		pickRandomMove(Player, PlayerMove, Dim).


gameOver(Player, Dim) :-
		allMoves(Player, Dim, _List, Len),
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
makeMove(Player, PlayCoords, PlayerOut) :- 
				(getSymbol(PlayCoords, 'empty')
					-> playerValue(Player, Symbol)
					 ; playerValueZ(Player, Symbol)),

				setSymbol(PlayCoords, Symbol),
				nextPlayer(Player, PlayerOut).

checkMove(Player, PlayCoords, Dim) :-
	getSymbol(PlayCoords, Content),
	(Content = 'empty' ; nextPlayer(Player, PlayerOut), playerValue(PlayerOut, Content)),
	retractall(visited(_)),
	checkMoveChain(Player, PlayCoords, Dim).


checkMoveChain(Player, [Row, Col], [Rows, Cols]) :- 
	not(visited([Row, Col])),
	assertz(visited([Row, Col])),

	Row >= 1, Row =< Rows, Col >=1, Col =< Cols,
	playerValue(Player, Symbol),
	playerValueZ(Player, SymbolZ),

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

playerValue(0, 'bAliv').
playerValue(1, 'rAliv').
playerValueZ(0, 'bDead').
playerValueZ(1, 'rDead').

nextPlayer(PlayerIn, PlayerOut) :- PlayerOut is (PlayerIn + 1) mod 2.