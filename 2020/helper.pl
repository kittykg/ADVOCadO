% nvim: set syntax=prolog

:- use_module(library(readutil)).

code_list_to_number(C, N) :-
    is_list(C), number_codes(N, C).

string_to_number(Str, N) :-
    string(Str), number_codes(N, Str).

read_number_line(Stream, N) :-
    read_line_to_codes(Stream, Codes),
    code_list_to_number(Codes, N).

read_string_file(Stream, []) :- at_end_of_stream(Stream).
read_string_file(Stream, [S|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, S),
    read_string_file(Stream, L).

read_number_file(Stream, []) :- at_end_of_stream(Stream).
read_number_file(Stream, [N|L]) :-
    \+ at_end_of_stream(Stream),
    read_number_line(Stream, N),
    read_number_file(Stream, L).
