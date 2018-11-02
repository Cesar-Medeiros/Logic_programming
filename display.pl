:-consult('board.pl').


% display_game(+Board)
%   Responsible for printing the board
display_game([Rows, Cols]) :- 

        printChar(' ', 4),
        printFirstLine(Rows, Cols),

        forall(between(1, Rows, N), (

                Row is Rows - N + 1,

                number_string(Row, S),
                writef('%3r ', [S]),

                printLine(Row, Rows, Cols),
                printChar(' ', 4),

                (Row \= 1 
                        -> printSeparationLine(Rows, Cols), !
                        ; printFinalLine(Rows, Cols))
        )),
        printChar(' ', 4),
        printCoordsLine(Rows, Cols).




% printLine(+Line)
%   Print a line decorated
printLine(Row, _, Cols) :- 
        put_code('┃'),
        put_char(' '),

        forall(between(1, Cols, Col), 
                (
                getSymbol([Row, Col], Content),
                printCell(Content),
                
                put_char(' '),
                (Col == Cols -> 
                        put_code('┃'), ! 
                        ; 
                        (put_code('│'),
                        put_char(' ')))
              
                )
              ),        
        nl.



% printCell(+Character)
%   Translate cell content to a symbol and prints it
printCell(C) :- 
        symbol(C,V), 
        put_code(V).


% printFirstLine(+Line)
%   Prints first decorative line
printFirstLine(_, Cols) :- 
        printChar('┏', 1),
        printChar('━', 3),
        N is Cols - 1,
        forall(between(1, N, _), 
                (printChar('┯', 1), 
                 printChar('━', 3)) 
              ),        
        printChar('┓', 1), nl.


% printSeparationLine(+Line)
%   Prints separation decorative line
printSeparationLine(_, Cols) :- 
        printChar('┠', 1),
        printChar('─', 3),
        N is Cols - 1,
        forall(between(1, N, _), 
                (printChar('┼', 1),
                 printChar('─', 3))
                ),            
        printChar('┨', 1), nl.



% printFinalLine(+Line)
%   Prints final decorative line
printFinalLine(_, Cols) :- 
        printChar('┗', 1),
        printChar('━', 3),
        N is Cols - 1,
        forall(between(1, N, _), 
                (printChar('┷', 1),
                printChar('━', 3))
                ),            
        printChar('┛', 1), nl.


% printCoordsLine(+Line)
%   Prints column coordenates line
printCoordsLine(_, Cols) :- 
        FinalCode is 97 + Cols - 1,
        forall(between(97, FinalCode, Code),
                (printChar(' ', 2), 
                 put_code(Code), 
                 printChar(' ', 1))
                ),
        nl.



% printChar(+Char, +N)
%   Print a Char N times
printChar(_, 0).
printChar(C, N) :- 
        put_char(C),
        N1 is N - 1,
        printChar(C, N1).


printPlayer(Player) :- 
        write('Player '),
        write(Player),
        nl.

% symbol(+String, -Symbol)
%   Translate internal representation to a symbol
symbol('empty', ' ').
symbol('rAliv', '◯').
symbol('rDead', '⨂').
symbol('bAliv', '⨉').
symbol('bDead', '●'). 