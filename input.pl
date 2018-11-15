getGameInfo(PlayersType, AIType, Dim) :-
    getPlayersType(PlayersType),
    getAIType(AIType),
    getDim(Dim).

getPlayersType(PlayersType) :-
    mainMenu,
	mainMenuInput(GameType),
	playersType(GameType, PlayersType).
    
mainMenuInput(N) :-
    input('Option', [N], checkMenuInput, 'Invalid Input').


getAIType(AIType) :-
    aiMenu,
	aiMenuInput(Level),
	aiType(Level, AIType).
    
aiMenuInput(N) :-
    input('Option', [N], checkMenuInput, 'Invalid Input').

checkMenuInput(N) :-
    N>=0,
    N=<3.

getDim(Dim) :-
    dimMenu,
    dimInput(NRows, NCols),
    Dim = [NRows, NCols].

dimInput(NRows, NCols) :-
    input('Number of Rows', [NRows], checkDim, 'Invalid number of rows'),
    input('Number of Collumns', [NCols], checkDim, 'Invalid number of columns').

checkDim(N) :- N > 0.

playersType(1, ['user', 'user']).
playersType(2, ['user', 'computer']).
playersType(3, ['computer', 'computer']).

aiType(1, 'random').
aiType(2, 'minimax').
aiType(3, 'minimax').


playInput(_-[NRows, NCols], [Row, Col]) :-
    input('Row', [Row, NRows], checkRows, 'Invalid Row'),
    input('Col', [CCol, NCols], checkCols, 'Invalid Col'),
    char_code('a', CodeA),
    char_code(CCol, CodeC),
    Col is CodeC - CodeA + 1.

checkRows([Row, NRows]) :-
    number(Row),
    Row>=1,
    Row=<NRows.


checkCols([Col, NCols]) :-
    is_alpha(Col),
    char_code('a', CodeA),
    char_code(Col, CodeC),
    CodeC >= CodeA,
    CodeC =< CodeA + NCols.


input(Prompt, [Value|Rest], CheckPred, ErrorMsg) :-
    repeat,
    format('~w: \n', [Prompt]),
    read(Value),

    (   call(CheckPred, [Value|Rest])
    ->  true, !
    ;   format('ERROR: ~w.~n', [ErrorMsg]),
        fail
    ).

    % (Type = 'String' -> read(Value), !;
    % (Type = 'Char'   -> read(Value), !;
    % (Type = 'Number' -> read(CValue), atom_number(CValue, Value)))), 