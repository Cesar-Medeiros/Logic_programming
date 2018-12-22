:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(random)).

:- consult(boards).
:- consult(display).

solve_2(Board) :-
    statistics(runtime, [T1|_]),
    getBoardDim(Board, Dim),
    
    generateLights(Lights, Dim),

    restrict_board_2(Board, Dim, Lights),
    write('.'),
    findAll(Val, member(cell(_, _, Val), Lights), FinalLights),
    labeling([], FinalLights),
    write('Result: '),
    write(Lights),
    nl,

    statistics(runtime, [T2|_]),
    T3 is T2 - T1,
    write('Time: '),
    write(T3),
    write(' ms'),
    nl,nl.

restrict_board_2([], _, _).

restrict_board_2([Cell | RestBoard], Dim, Lights) :-

    Cell = cell(Row, Col, Val),
    
    restrict_orthogonally(Row, Col, Dim, Lights, ResT, ResB, ResL, ResR),

    Val #= ResT + ResB + ResR + ResL + ResURD + ResULD + ResBRD + ResBLD,

    restrict_board_2(RestBoard, Dim, Lights).

restrict_orthogonally(Row, Col, Dim, Lights, ResT, ResB, ResL, ResR) :-
    Max is Dim+1,
    write(Row),
    write(','),
    write(Col),
    nl,
    member(cell(0, Col, TVal), Lights),
    member(cell(Max, Col, BVal), Lights),
    member(cell(Row, Max, RVal), Lights),
    member(cell(Row, 0, LVal), Lights),

    Row #=< TVal #<=> ResT,
    Row #>= (Dim + 1 - BVal) #<=> ResB,

    Col #=< LVal #<=> ResL,
    Col #>= (Dim + 1 - RVal) #<=> ResR.

restrict_diagonally(Row, Col, Dim, Lights, ResURD, ResULD, ResBRD, ResBLD) :-
    calculate_ortho_dists(Row, Col, Dim,LeftDist, RightDist, UpDist, BottomDist),
    
    restrict_up_right_diag(UpDist, RightDist, Lights, ResURD, Row, Col), 
    
    restrict_up_left_diag(UpDist, LeftDist, Lights, ResULD, Row, Col), 
   
    restrict_bottom_right_diag(BottomDist, RightDist, Lights, ResBRD, Row, Col), 
  
    restrict_bottom_left_diag(BottomDist, LeftDist, Lights, ResBLD, Row, Col).

restrict_up_right_diag(UpDist, RightDist, Lights, ResURD, Row, Col) :-
    min(UpDist, RightDist, Min),
    UpRightDiagRow is Row - Min,
    UpRightDiagCol is Col + Min,
    (member(cell(UpRightDiagRow, UpRightDiagCol, Val), Lights), !,
     Min #>= Val #<=> ResURD
     ;
     ResURD #= 0).

restrict_up_left_diag(UpDist, LeftDist, Lights, ResULD, Row, Col) :-
    min(UpDist, LeftDist, Min),
    UpLeftDiagRow is Row - Min,
    UpLeftDiagCol is Col - Min,
    (member(cell(UpLeftDiagRow, UpLeftDiagCol, Val), Lights), !,
     Min #>= Val #<=> ResULD
     ;
     ResULD #= 0).

restrict_bottom_right_diag(BottomDist, RightDist, Lights, ResBRD, Row, Col) :-
    min(BottomDist, RightDist, Min),
    BottomRightDiagRow is Row + Min,
    BottomRightDiagCol is Col + Min,
    (member(cell(BottomRightDiagRow, BottomRightDiagCol, Val), Lights), !,
     Min #>= Val #<=> ResBRD
     ;
     ResBRD #= 0).

restrict_bottom_left_diag(BottomDist, LeftDist, Lights, ResBLD, Row, Col) :-
    min(BottomDist, LeftDist, Min),
    BottomLeftDiagRow is Row + Min,
    BottomLeftDiagCol is Col - Min,
    (member(cell(BottomLeftDiagRow, BottomLeftDiagCol, Val), Lights), ! , 
     Min #>= Val #<=> ResBLD
     ;
     ResBLD #= 0).

calculate_ortho_dists(Row, Col, Dim, LeftDist, RightDist, UpDist, BottomDist) :-
    LeftDist is Col,
    RightDist is Dim + 1 - Col,
    UpDist is Row,
    BottomDist is Dim + 1 - Row.

generateLights(Lights, Dim) :-
    generateTopLights(TopLights, Dim),
    generateBottomLights(BottomLights, Dim),
    generateLeftLights(LeftLights, Dim),
    generateRightLights(RightLights, Dim),
    append([TopLights, BottomLights, LeftLights, RightLights], Lights).

generateTopLights(Lights, Dim) :-   
    Value in 0..Dim,
    findall(cell(0, Val, Value), between(1, Dim, Val), Lights).

generateBottomLights(Lights, Dim) :-
    Max is Dim + 1,
    Value in 0..Dim,
    findall(cell(Max, Val, Value), between(1, Dim, Val), Lights).

generateLeftLights(Lights, Dim) :-
    Value in 0..Dim,
    findall(cell(Val, 0, Value), between(1, Dim, Val), Lights).

generateRightLights(Lights, Dim) :-
    Max is Dim + 1,
    Value in 0..Dim,
    findall(cell(Val, Max, Value), between(1, Dim, Val), Lights).
    

getBoardDim(Board, Dim) :-
    length(Board, Size),
    Dim is floor(sqrt(Size)).

min(Num1, Num2, Num1) :- Num1 < Num2.
min(Num1, Num2, Num2).

