mainMenuInput(N) :-
    input('Option', [N], checkMenuInput, 'Invalid Input').

checkMenuInput(N) :-
    N >= 0,
    N =< 3.


playInput([NRows, NCols], [Row, Col]) :-
    input('Row', [Row, NRows], checkRows, 'Invalid Row'),
    input('Col', [Col, NCols], checkCols, 'Invalid Col').

checkRows([Row, NRows]) :-
    Row >= 1,
    Row =< NRows.


checkCols([Col, NCols]) :-
        Col @>= 'a',
        Col @=< ('a' + NCols).


input(Prompt, [Value | Rest], CheckPred, ErrorMsg) :-
    repeat,
    format('~w: ', [Prompt]),
    read(Value),

    % (Type = 'String' -> read(Value), !;
    % (Type = 'Char'   -> read(Value), !;
    % (Type = 'Number' -> read(CValue), atom_number(CValue, Value)))),    
    
    (   call(CheckPred, [Value | Rest])
    ->  true, !
    ;   format('ERROR: ~w.~n', [ErrorMsg]),
        fail
    ).