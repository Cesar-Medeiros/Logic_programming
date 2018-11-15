mainMenu :-
    write('┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n'),
    write('┃           Virus Wars          ┃\n'),
    write('┠───────────────────────────────┨\n'),
    write('┃                               ┃\n'),
    write('┃  (1) - Player vs Player       ┃\n'),
    write('┃  (2) - Player vs Computer     ┃\n'),
    write('┃  (3) - Computer vs Computer   ┃\n'),
    write('┃                               ┃\n'),
    write('┃  (0) - Exit                   ┃\n'),
    write('┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n\n').

aiMenu :-
    write('┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n'),
    write('┃           Difficulty          ┃\n'),
    write('┠───────────────────────────────┨\n'),
    write('┃                               ┃\n'),
    write('┃  (1) - Level 1                ┃\n'),
    write('┃  (2) - Level 2                ┃\n'),
    write('┃  (3) - Level 3                ┃\n'),
    write('┃                               ┃\n'),
    write('┃  (0) - Exit                   ┃\n'),
    write('┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n\n').

dimMenu :-
    write('┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n'),
    write('┃        Board Dimensions       ┃\n'),
    write('┠───────────────────────────────┨\n').


printPlayer(Player) :-
    getPlayerSymbol(Player, Symbol),
    format('~n--------------- Player ~w ---------------~n', [Symbol]).


getPlayerSymbol(Player, Symbol) :- playerValue(Player, Value), symbol(Value, Symbol).

cls:-write('\e[H\e[2J').
