% nvim: set syntax=prolog

:- use_module(library(readutil)).
:- use_module(library(clpfd)).

% ------------------ I/O ------------------

code_list_to_number(C, N) :-
    is_list(C), number_codes(N, C).

string_to_number(S, N) :-
    number_codes(N, S).

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

prod_list(L, Prod) :- foldl(mult_, L, 1, Prod).

enum_list(L, Idx) :-
    length(L, Len), Upper is Len - 1,
    numlist(0, Upper, Idx).

head([H|_], H).
last(L, Last) :- append(_, [Last], L).

any(Goal, L) :- include(Goal, L, FL), FL \== [].

flatten([], []) :- !.
flatten([L|R], Res) :-
    !,
    flatten(L, NewL),
    flatten(R, NewR),
    append(NewL, NewR, Res).
flatten(L, [L]).

take(N, List, FL) :- split_at(N, List, FL, _).

zip([], [], []).
zip([X1|R1], [X2|R2], [(X1, X2)|R]) :-
    zip(R1, R2, R).

unzip([], [], []).
unzip([(X, Y)|R], [X|R1], [Y|R2]) :-
    unzip(R, R1, R2).

uneven_zip(L1, [], []) :-
    length(L1, Len), Len #>= 1.
uneven_zip([], L2, []) :-
    length(L2, Len), Len #>= 1.
uneven_zip([X1|R1], [X2|R2], [(X1, X2)|L]) :-
    uneven_zip(R1, R2, L).

% ------------------ Dict ------------------

get_all_val(Dict, Vals) :-
    dict_keys(Dict, Keys), maplist(get_val_(Dict), Keys, Vals).
get_val_(Dict, K, V) :- V = Dict.get(K).

in_dict(Dict, K) :-
    dict_keys(Dict, Keys), member(K, Keys), !.

% ------------------ Trie ------------------

get_trie_keys(T, Keys) :-
    bagof(Key, trie_gen(T, Key), Keys).

get_trie_vals(T, Vals) :-
    get_trie_keys(T, Keys),
    maplist(trie_lookup(T), Keys, Vals).

get_trie_items(T, Items) :-
    get_trie_keys(T, Keys),
    maplist(trie_lookup(T), Keys, Vals),
    zip(Keys, Vals, Items).

get_counter_trie(L, T) :-
    trie_new(T),
    get_counter_trie_(L, T).

get_counter_trie_([], _).
get_counter_trie_([C|R], T) :-
    (trie_gen(T, C), ! ->
        trie_lookup(T, C, Val),
        NewVal #= Val + 1,
        trie_update(T, C, NewVal);
        trie_insert(T, C, 1)
    ),
    get_counter_trie_(R, T).

print_trie(T) :-
    get_trie_items(T, Items),
    maplist(print_trie_, Items).
print_trie_(Item) :-
    term_string(Item, String),
    writeln(String).

% ------------------ Misc ------------------

mult_(X, Y, Res) :- Res is X * Y.
minus(X, Y, Res) :- Res is X - Y.
