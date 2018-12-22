:-use_module(library(csv)).
:-use_module(library(file_systems)).


main :-
open('test.csv', write, Stream),
write_record(Stream,[integer(1), integer(2)]),
close(Stream).


% main :-
%     open('convertcsv.csv', read, Stream),
%     read_record(Stream, Record), 
%     write(Record),
    
%     read_record(Stream, Record1), 
%     write(Record1).


save_test_csv().