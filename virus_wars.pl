:- consult('display.pl').
:- consult('board.pl').
:- consult('input.pl').
:- consult('menu.pl').
:- consult('ai.pl').
:- dynamic('visited/1').
:- dynamic('aiLevel/1').
:- dynamic('playerType/2').



% =====
% Play
% =====

play :- 
		getGameInfo(PlayersType, FirstPlayer, AI, Dim),
		storeGameInfo(PlayersType, AI),
		createBoard(BoardCells, Dim),
		Board = BoardCells-Dim,
		game(FirstPlayer, Board).

storeGameInfo([Player1Type, Player2Type], AI) :-
		asserta(playerType(0, Player1Type)),
		asserta(playerType(1, Player2Type)),
		asserta(aiLevel(AI)).



% =====
% Game
% =====

game(Player, Board) :- 
		game_over(Board, Player, Winner),
		getPlayerSymbol(Winner, Symbol),
		display_game(Player, Board),
		format('Player ~w won.~n', [Symbol]).

game(Player, Board) :- 
		display_game(Player, Board),
		playerType(Player, PlayerType),
		move(Player, PlayerType, Board, NewBoard, NewPlayer),
		game(NewPlayer, NewBoard).

game_over(Board, Player, Winner):-
		valid_moves(Board, Player, ListOfMoves),
		length(ListOfMoves, Len), !,
		Len = 0,
		opponent(Player, Winner).
	

% =====
% Move
% =====

move(Player, 'user', Board, NewBoard, NewPlayer):-
		playInput(Board, Move),
		valid_move(Board, Player, Move),
		makeMove(Board, Player, Move, NewBoard, NewPlayer).

move(Player, 'user', Board, Board, Player):-
		write('\nInvalid Move\n').

move(Player, 'computer', Board, NewBoard, NewPlayer):- 
		aiLevel(AILevel),
		choose_move(Board, Player, AILevel, Move),
		makeMove(Board, Player, Move, NewBoard, NewPlayer).


% ==========
% Make Move
% ==========

% makeMove(+Board, +Player, -NewBoard, -NewPlayer)
%   Responsible for making a move and return the next board and player
makeMove(Board, Player, Move, NewBoard, NewPlayer) :- 
		getSymbol(Board, Move, CurrentSymbol), 
		getNewSymbol(Player, CurrentSymbol, NewSymbol),
		setSymbol(Board, Move, NewSymbol, NewBoard),
		nextPlayer(Player, NewPlayer).


% ===========
% New Symbol
% ===========
getNewSymbol(Player, 'empty', NewSymbol):-
	playerValue(Player, NewSymbol).

getNewSymbol(Player, CurrentSymbol, NewSymbol):-
	opponent(Player, Opponent),
	playerValue(Opponent, OpponentSymbol),
	CurrentSymbol = OpponentSymbol,
	playerValueZ(Player, NewSymbol).


% ============
% Choose Move
% ============

choose_move(Board, Player, 1, Move) :-
	valid_moves(Board, Player, ListOfMoves),
    length(ListOfMoves, Len),
    random(0, Len, Random),
    nth0(Random, ListOfMoves, Move).

choose_move(Board, Player, 2, Move):-
	ai(Board, Player, 1, Move).

choose_move(Board, Player, 3, Move):-
	ai(Board, Player, 3, Move).


% ===========
% Check Move
% ===========

valid_move(Board, Player, Move) :-
	(getSymbol(Board, Move, 'empty') 
	; 
	(opponent(Player, Opponent), playerValue(Opponent, OpponentSymbol), getSymbol(Board, Move, OpponentSymbol))),
	retractall(visited(_)),
	checkMoveChain(Player, Move, Board).

checkMoveChain(Player, [Row, Col], Board) :- 
	not(visited([Row, Col])),
	asserta(visited([Row, Col])),

	playerValue(Player, Symbol),
	playerValueZ(Player, SymbolZ),

	between(-1, 1, ROffset), between(-1, 1, COffset),
	not((ROffset = 0, COffset = 0)),
	NRow is Row + ROffset, 
	NCol is Col + COffset, 
	isPositionValid(Board, [NRow, NCol]),
	(getSymbol(Board, [NRow, NCol], Symbol) 
	; 
	(getSymbol(Board, [NRow, NCol], SymbolZ), checkMoveChain(Player, [NRow, NCol], Board))).


valid_moves(Board, Player, ListOfMoves) :-
    findall(Move, valid_move(Board,Player, Move), ListOfMoves).



% ============
% Utilities
% ============

opponent(Player, Opponent) :- Opponent is (Player + 1) mod 2.
nextPlayer(PlayerIn, NewPlayer) :- NewPlayer is (PlayerIn + 1) mod 2.