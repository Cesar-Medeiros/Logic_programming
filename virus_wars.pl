:- consult('display.pl').
:- consult('board.pl').
:- consult('input.pl').
:- consult('menu.pl').
:- consult('ai.pl').
:- dynamic('visited/1').
:- dynamic('aiType/1').
:- dynamic('playerType/2').


play :- 
			getGameInfo(PlayersType, FirstPlayer, AI, Dim),
			storeGameInfo(PlayersType, AI),
			createBoard(BoardCells, Dim),
			Board = BoardCells-Dim,
			game(FirstPlayer, Board).


storeGameInfo([Player1Type, Player2Type], AI) :-
			asserta(playerType(0, Player1Type)),
			asserta(playerType(1, Player2Type)),
			asserta(aiType(AI)).

game(Player, Board) :- 
			gameOver(Player, Board).

game(Player, Board) :- 
			display_game(Player, Board),
			playerType(Player, PlayerType),
			move(Player, PlayerType, Board, BoardOut, PlayerOut),
			game(PlayerOut, BoardOut).

gameOver(Player, Board) :-
	not(generateValidMoves(Player, _, Board)),
	nextPlayer(Player, PreviousPlayer),
	getPlayerSymbol(PreviousPlayer, Symbol),
	format('Player ~w won.~n', [Symbol]).



move(Player, 'user', Board, BoardOut, PlayerOut):-
		playInput(Board, PlayerMove), !,
		(checkMove(Player, PlayerMove, Board)
			-> (/*write('\nValid Move\n'), write(PlayerMove),nl, */
			makeMove(Board, Player, PlayerMove, BoardOut, PlayerOut))
			 ; write('\nInvalid Move\n'), BoardOut = Board, PlayerOut is Player).

move(Player, 'computer', Board, BoardOut, PlayerOut):- 
	aiType('level1'),
	pickRandomMove(Player, PlayerMove, Board),
	makeMove(Board, Player, PlayerMove, BoardOut, PlayerOut),
	nextPlayer(Player, PlayerOut).

move(Player, 'computer', Board, BoardOut, PlayerOut):- 
		aiType('level3'),
		minimax(Board, Player, BoardOut, _Val, 6),
		nextPlayer(Player, PlayerOut).


% makeMove(+Board, +Player, -BoardOut, -PlayerOut)
%   Responsible for making a move and return the next board and player
makeMove(Board, Player, PlayCoords, BoardOut, PlayerOut) :- 
				(getSymbol(Board, PlayCoords, 'empty')
					-> playerValue(Player, Symbol)
					 ; playerValueZ(Player, Symbol)),
				setSymbol(Board, PlayCoords, Symbol, BoardOut),
				nextPlayer(Player, PlayerOut).

checkMove(Player, PlayCoords, Board) :-
	isPositionValid(Board, PlayCoords),
	(getSymbol(Board, PlayCoords, 'empty') ; (nextPlayer(Player, PlayerOut), playerValue(PlayerOut, Content), getSymbol(Board, PlayCoords, Content))),
	retractall(visited(_)),
	checkMoveChain(Player, PlayCoords, Board).
	% (checkMoveChain(Player, PlayCoords, Board, Dim), !, write('OK'), write(PlayCoords); (write('Fail'), write(PlayCoords), fail)).

checkMoveChain(Player, [Row, Col], Board) :- 
	not(visited([Row, Col])),
	asserta(visited([Row, Col])),

	playerValue(Player, Symbol),
	playerValueZ(Player, SymbolZ),

	%Perhaps the use of BFS insted of DFS is more efficient
	between(-1, 1, ROffset), between(-1, 1, COffset),
	not((ROffset = 0, COffset = 0)),
	NRow is Row + ROffset, 
	NCol is Col + COffset, 
	isPositionValid(Board, [NRow, NCol]),
	(getSymbol(Board, [NRow, NCol], Symbol) 
	; 
	(getSymbol(Board, [NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], Board))).



generateValidMoves(Player, List, Board) :-
	playerValue(Player, Value),
	nextPlayer(Player, Player2),
	playerValue(Player2, Value2),
	setof([NRow, NCol],
		(ROffset, COffset)^(getSymbol(Board,[Row, Col], Value),
		between(-1, 1, ROffset), between(-1, 1, COffset),
		not((ROffset = 0, COffset = 0)),
		NRow is Row + ROffset, 
		NCol is Col + COffset,
		isPositionValid(Board, [NRow, NCol]),
		(getSymbol(Board, [NRow, NCol], 'empty');
		getSymbol(Board, [NRow, NCol], Value2))),
		List),
	length(List, Len),
	!,
	Len \= 0.


% generateValidMove(Player, Board, Move) :- 


adjacentPosition(Player, Board, [Row, Col], [NRow, NCol]) :-
	between(-1, 1, ROffset), between(-1, 1, COffset),
	not((ROffset = 0, COffset = 0)),
	NRow is Row + ROffset, 
	NCol is Col + COffset,
	isPositionValid(Board, [NRow, NCol]),
	(getSymbol(Board, [NRow, NCol], 'empty');
	getSymbol(Board, [NRow, NCol], Value2)).
		


            
nextPlayer(PlayerIn, PlayerOut) :- PlayerOut is (PlayerIn + 1) mod 2.