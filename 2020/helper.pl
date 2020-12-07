% nvim: set syntax=prolog

:- use_module(library(readutil)).


% ------------------ IO ------------------
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

% --------------- High-order functions ---------------
include_with_arg(Goal, List, Arg2, Included) :-
    include_with_arg_(List, Arg2, Goal, Included).
    
include_with_arg_([], _, _, []).
include_with_arg_([Elem|Tail], Arg2, Goal, Included) :-
    ( call(Goal, Elem, Arg2)
    -> Included = [Elem|Included1]
    ;  Included = Included1
    ),
    include_with_arg_(Tail, Arg2, Goal, Included1).

map_with_arg(Goal, List, Arg1, Res) :- map_with_arg_(List, Arg1, Goal, Res).

map_with_arg_([], _, _, []).
map_with_arg_([Elem|Tail], Arg1, Goal, Res) :-
    call(Goal, Elem, Arg1, SubRes),
    map_with_arg_(Tail, Arg1, Goal, Others),
    Res = [SubRes|Others].

% ------------------ Tuples ------------------
first((X, _), X).
second((_, Y), Y).

% ------------------ Misc ------------------
sum(X, Y, Res) :- Res is X + Y.
mult(X, Y, Res) :- Res is X * Y.
