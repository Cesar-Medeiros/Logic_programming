% ==========
% Game Info
% ==========

getGameInfo(PlayersType, FirstPlayer, AILevel, Dim) :-
    getPlayersType(PlayersType),
    getFirstPlayer(FirstPlayer, PlayersType),
    getAILevel(AILevel, PlayersType),
    getDim(Dim).



% ============
% Player Type
% ============

getPlayersType(PlayersType) :-
    mainMenu,
	mainMenuInput(GameType),
	playersType(GameType, PlayersType).
    
mainMenuInput(N) :-
    input('Option', [N], checkMainMenuInput, 'Invalid Input').

checkMainMenuInput([N]) :-
    integer(N),
    N>=0,
    N=<3.

playersType(1, ['user', 'user']).
playersType(2, ['user', 'computer']).
playersType(3, ['computer', 'computer']).


% =============
% First Player
% =============

getFirstPlayer(FirstPlayer, PlayersType) :-
    firstPlayerMenu(PlayersType),
    firstPlayerMenuInput(FirstPlayer).

firstPlayerMenuInput(FirstPlayer) :-
    input('Option', [PlayerOption], checkPlayerInput, 'Invalid Input'),
    FirstPlayer is PlayerOption - 1.

checkPlayerInput([Player]) :-
    integer(Player),
    Player>=1,
    Player=<2.

% ========
% AI Level
% ========

getAILevel(0, ['user', 'user']).
getAILevel(AILevel, _PlayersType) :-
    aiMenu,
	aiMenuInput(AILevel).

aiMenuInput(N) :-
    input('Option', [N], checkMenuInput, 'Invalid Input').

checkMenuInput([N]) :-
    integer(N),
    N>=1,
    N=<3.

aiType(1, 'level1').
aiType(2, 'level2').
aiType(3, 'level3').



% ================
% Board Dimension
% ================

getDim(Dim) :-
    dimMenu,
    dimInput(NRows, NCols),
    Dim = [NRows, NCols].

dimInput(NRows, NCols) :-
    input('Number of Rows', [NRows], checkDim, 'Invalid number of rows'),
    input('Number of Columns', [NCols], checkDim, 'Invalid number of columns').

checkDim([N]) :- 
    integer(N),
    N > 1, 
    N < 15.


% ===========
% Move input
% ===========

playInput(_-[NRows, NCols], [Row, Col]) :-
    input('Row', [Row, NRows], checkRows, 'Invalid Row'),
    input('Col', [CCol, NCols], checkCols, 'Invalid Col'),
    char_code('a', CodeA),
    char_code(CCol, CodeC),
    Col is CodeC - CodeA + 1.

checkRows([Row, NRows]) :-
    integer(Row),
    Row>=1,
    Row=<NRows.

checkCols([Col, NCols]) :-
    atom(Col),
    atom_length(Col, Len),
    Len = 1,
    char_type(Col, lower),
    char_code('a', CodeA),
    char_code(Col, CodeC),
    CodeC >= CodeA,
    CodeC =< CodeA + NCols.


% ===============
% Input template
% ===============
input(Prompt, [Value|Rest], CheckPred, ErrorMsg) :-
    repeat,
    format('~w: \n', [Prompt]),
    
    (   catch(read(Value), _, fail), call(CheckPred, [Value|Rest])
    ->  true, !
    ;   format('ERROR: ~w.~n', [ErrorMsg]),
        fail
    ).