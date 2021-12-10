% nvim: set syntax=prolog

% AoC 2021 Day 8

:- consult('../helper.pl').
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).


%-----------------------------------%
%            IO parsing             %
%-----------------------------------%

% split_delimiter(+String, -ListPair)
split_delimiter(S, [L1, L2]) :-
    split_string(S, " ", "", LS),
    nth0(I, LS, "|"),
    split_at(I, LS, L1, [_|L2]).

% get_string_list(-ListPairs)
get_string_list(L) :-
    open('input_08', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(split_delimiter, SL, L).

%-----------------------------------%
%              Task 1               %
%-----------------------------------%

% is_1(+String)
is_1(X) :-
    string_codes(X, C),
    length(C, 2).

% is_4(+String)
is_4(X) :-
    string_codes(X, C),
    length(C, 4).

% is_7(+String)
is_7(X) :-
    string_codes(X, C),
    length(C, 3).

% is_8(+String)
is_8(X) :-
    string_codes(X, C),
    length(C, 7).

% check_1478(+ListPair, -Count)
check_1478([_, L2], C) :-
    include(\X^(is_1(X);is_4(X);is_7(X);is_8(X)), L2, FL),
    length(FL, C).

task_1(Out) :-
    get_string_list(L),
    maplist(check_1478, L, Counts),
    sum_list(Counts, Out).

%-----------------------------------%
%              Task 2               %
%-----------------------------------%

% sorted_string_code(+String, -SortedCodes)
sorted_string_code(X, SC) :-
    string_codes(X, C), sort(0, @<, C, SC).

% get_1(+ListOfCodes, -SortedStringCode)
get_1(L1, SC) :-
    include(\X^(length(X, 2)), L1, [SC]).

% get_4(+ListOfCodes, -SortedStringCode)
get_4(L1, SC) :-
    include(\X^(length(X, 4)), L1, [SC]).

% get_7(+ListOfCodes, -SortedStringCode)
get_7(L1, SC) :-
    include(\X^(length(X, 3)), L1, [SC]).

% get_8(+ListOfCodes, -SortedStringCode)
get_8(L1, SC) :-
    include(\X^(length(X, 7)), L1, [SC]).

% get_1478(+Trie, +ListOfCodes, -CodeFor1,
%          -CodeFor4, -CodeFor7, -CodeFor8)
get_1478(T, L, C1, C4, C7, C8) :-
    get_1(L, C1),
    trie_insert(T, C1, 1),
    get_4(L, C4),
    trie_insert(T, C4, 4),
    get_7(L, C7),
    trie_insert(T, C7, 7),
    get_8(L, C8),
    trie_insert(T, C8, 8).

% length_5_digit(+Code, +CodeFor1, +CodeFor6, +Trie, -Num)
length_5_digit(C, C1, C6, T, N) :-
    (subset(C1, C) ->
        N = 3;
        (subset(C, C6) ->
            N = 5;
            N = 2
        )
    ), trie_insert(T, C, N).

% length_5_digit(+Code, +CodeFor4, +CodeFor7, +Trie, -Num)
length_6_digit(C, C4, C7, T, N) :-
    (subset(C7, C) ->
        (subset(C4, C) ->
            N = 9;
            N = 0
        );
        N = 6
    ), trie_insert(T, C, N).

%decode(+ListOfCodes, -Trie)
decode(L1, T) :-
    maplist(sorted_string_code, L1, SortedL1),
    trie_new(T),
    get_1478(T, SortedL1, C1, C4, C7, _),
    include(\X^(length(X, 6)), SortedL1, Len6),
    maplist(\X^(length_6_digit(X, C4, C7, T)), Len6, NumsLen6),
    nth0(I, NumsLen6, 6), nth0(I, Len6, C6),
    include(\X^(length(X, 5)), SortedL1, Len5),
    maplist(\X^(length_5_digit(X, C1, C6, T)), Len5, _).

% gen_output(+ListOfCodes, +Trie, -Output)
gen_output(L2, T, Out) :-
    maplist(sorted_string_code, L2, SortedL2),
    reverse(SortedL2, RSortedL2),
    gen_output_(RSortedL2, T, 0, Out).

gen_output_([C], T, Power, Out) :-
    trie_lookup(T, C, Digit),
    Out #= Digit * (10 ^ Power).
gen_output_([C|R], T, Power, Out) :-
    trie_lookup(T, C, Digit),
    N #= Digit * (10 ^ Power),
    PowerN #= Power + 1,
    gen_output_(R, T, PowerN, Res),
    Out #= Res + N.

% line_output(+ListPair, -Output)
line_output([L1, L2], Out) :-
    decode(L1, T),
    gen_output(L2, T, Out).

task_2(Out) :-
    get_string_list(L),
    maplist(line_output, L, Outputs),
    sumlist(Outputs, Out).
