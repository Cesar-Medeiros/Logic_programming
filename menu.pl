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


printPlayer(Player) :-
    getPlayerSymbol(Player, Symbol),
    format('~n--------------- Player ~w ---------------~n', [Symbol]).


getPlayerSymbol(Player, Symbol) :- playerValue(Player, Value), symbol(Value, Symbol).

cls:-write('\e[H\e[2J').
