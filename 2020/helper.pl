% nvim: set syntax=prolog

:- use_module(library(readutil)).

% ------------------ I/O ------------------

code_list_to_number(C, N) :-
    is_list(C), number_codes(N, C).

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

prod_list(L, Prod) :- foldl(mult, L, 1, Prod).

enum_list(L, Idx) :-
    length(L, Len), Upper is Len - 1,
    numlist(0, Upper, Idx).

tail(L, T) :- append(_, [T], L).

% ------------------ Dict ------------------

get_all_val(Dict, Vals) :-
    dict_keys(Dict, Keys), maplist(get_val_(Dict), Keys, Vals).
get_val_(Dict, K, V) :- V = Dict.get(K).

% ------------------ Misc ------------------

sum(X, Y, Res) :- Res is X + Y.
mult(X, Y, Res) :- Res is X * Y.
minus(X, Y, Res) :- Res is X - Y.
