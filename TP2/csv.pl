:-use_module(library(csv)).
:-use_module(library(file_systems)).
:-use_module(library(codesio)).

save_test_csv(Stream, [Op1, Op2, Op3], Time) :-
    write_to_codes(Op1, CodeList1),
    write_to_codes(Op2, CodeList2),
    write_to_codes(Op3, CodeList3),
    write_record(Stream, [string(CodeList1), string(CodeList2), string(CodeList3), integer(Time)]).

open_test_csv(Stream) :-
    open('test.csv', write, Stream).
