:-use_module(library(lists)).
:-use_module(library(aggregate)).

:- consult(boards).


printBoard(Board, TopRow, BottomRow, RightRow, LeftRow) :-
        getBoardDim(Board, Dim),
        
        Rows = Dim,
        Cols = Dim,

        % printChar(0x0020, 4),
        % printFirstLine([Rows, Cols]),

        printChar(0x0020, 4),
        printHorOutLine(TopRow),

        
        % printChar(0x0020, 4),
        printFirstLine([Rows, Cols]),

        forall(between(1, Rows, N), (

                % printChar(0x0020, 4),
                put_code(0x2503),
                put_code(0x0020),
                nth1(N, LeftRow, LeftRowCell),
                write(LeftRowCell),
                put_code(0x0020),

                printLine(Board, N, [Rows, Cols]),

                
                put_code(0x0020),
                nth1(N, RightRow, RightRowCell),
                write(RightRowCell),
                put_code(0x0020),
                put_code(0x2503),
                nl,

                (N \= Rows 
                        -> printSeparationLine([Rows, Cols]), !
                        ; printFinalLine([Rows, Cols]))
        )),

        printChar(0x0020, 4),
        printHorOutLine(BottomRow)
        % printChar(0x0020, 4),
        % printFinalLine([Rows, Cols])
        .


% printLine(+Line)
%   Print a line decorated
printLine(Board, Row, [_, Cols]) :- 
        put_code(0x2503),
        put_code(0x0020),

        forall(between(1, Cols, Col), 
                (
                getCell(Board, Row, Col, Content),
                write(Content),
                
                put_code(0x0020),
                (Col == Cols -> 
                        put_code(0x2503), ! 
                        ; 
                        (put_code(0x2502),
                        put_code(0x0020)))
              
                )
              ).

% printLine(+Line)
%   Print a line decorated
printHorOutLine(List) :- 
        put_code(0x2503),
        put_code(0x0020),

        length(List, Len),

        forall(nth1(Index, List, Val), 
                (
                write(Val),
                
                put_code(0x0020),
                (Index == Len -> 
                        put_code(0x2503), ! 
                        ; 
                        (put_code(0x2502),
                        put_code(0x0020)))
              
                )
              ),        
        nl.


% printFirstLine(+Line)
%   Prints first decorative line
printFirstLine([_, Cols]) :- 
        
        printChar(0x250F, 1),
        printChar(0x2501, 3),
        printChar(0x254B, 1),
        printChar(0x2501, 3),

        N is Cols - 1,
        forall(between(1, N, _), 
                (printChar(0x253F, 1), 
                 printChar(0x2501, 3)) 
              ),

        printChar(0x254B, 1), 
        printChar(0x2501, 3),
        printChar(0x2513, 1),
        nl.


% printSeparationLine(+Line)
%   Prints separation decorative line
printSeparationLine([_, Cols]) :-

        printChar(0x2523, 1),
        printChar(0x2501, 3),
        printChar(0x2549, 1),
        printChar(0x2500, 3),
        N is Cols - 1,
        forall(between(1, N, _), 
                (printChar(0x253C, 1),
                 printChar(0x2500, 3))
                ),            
        printChar(0x254A, 1),
        printChar(0x2501, 3),
        printChar(0x252B, 1),
        nl.



% printFinalLine(+Line)
%   Prints final decorative line
printFinalLine([_, Cols]) :- 

        printChar(0x2517, 1),
        printChar(0x2501, 3),
        printChar(0x254B, 1),
        printChar(0x2501, 3),

        N is Cols - 1,
        forall(between(1, N, _), 
                (printChar(0x253F, 1),
                printChar(0x2501, 3))
                ),      
        
        printChar(0x254B, 1),
        printChar(0x2501, 3),
        printChar(0x251B, 1),
        nl.


% printChar(+Char, +N)
%   Print a Char N times
printChar(_, 0).
printChar(C, N) :- 
        put_code(C),
        N1 is N - 1,
        printChar(C, N1).



printBoardRepresentation(Board) :-
        forall(member(Elem, Board), (write(Elem), write(',') ,nl)).