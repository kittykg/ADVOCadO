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

% ------------------ List ------------------
split_at(N, List, SubList1, SubList2) :-
    length(SubList1, N),
    append(SubList1, SubList2, List).

sub_list(List, Start, End, Sub) :-
    length(List, Len), Start >= 0, End =< Len,
    SubLen is End - Start,
    append(T1, Sub, T2),
    length(T1, Start), length(Sub, SubLen),
    append(T2, _, List).

count(E, L, N) :-
    include(=(E), L, L2), length(L2, N).

repeat(Elem, N, L) :- repeat_(Elem, N, L).

repeat_(_, 0, []).
repeat_(Elem, N, [Elem|L]) :-
    N > 0, NextN is N - 1, 
    repeat_(Elem, NextN, L).

% ------------------ Misc ------------------
sum(X, Y, Res) :- Res is X + Y.
mult(X, Y, Res) :- Res is X * Y.
minus(X, Y, Res) :- Res is X - Y.
