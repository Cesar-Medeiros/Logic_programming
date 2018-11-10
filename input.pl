mainMenuInput(N) :-
    input('Option', [N], checkMenuInput, 'Invalid Input').

checkMenuInput(N) :-
    N>=0,
    N=<3.


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
    format('~w: ', [Prompt]),
    read(Value),

    (   call(CheckPred, [Value|Rest])
    ->  true, !
    ;   format('ERROR: ~w.~n', [ErrorMsg]),
        fail
    ).

    % (Type = 'String' -> read(Value), !;
    % (Type = 'Char'   -> read(Value), !;
    % (Type = 'Number' -> read(CValue), atom_number(CValue, Value)))), 