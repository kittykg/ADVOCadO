% nvim: set syntax=prolog

% AoC 2022 Day 3

:- consult('../helper.pl').

% code_property(+Code, -Value)
code_property(N, V) :-
    N #>= 97,
    N #=< 122,
    V #= N - 96.
code_property(N, V) :-
    N #>= 65,
    N #=< 90,
    V #= N - 38.

% shared_item(+String, -Value)
shared_item(S, V) :-
    string_codes(S, L),
    append(L1, L2, L),
    length(L1, X),
    length(L2, X),
    member(Shared, L1),
    member(Shared, L2),
    code_property(Shared, V).

% shared_item_group(+String1, +String2, +String3, -Value)
shared_item_group(S1, S2, S3, V) :-
    string_codes(S1, L1),
    string_codes(S2, L2),
    string_codes(S3, L3),
    member(Shared, L1),
    member(Shared, L2),
    member(Shared, L3),
    code_property(Shared, V).

% get_input(-Lines)
get_input(L) :-
    open('input_03', read, Stream),
    read_string_file(Stream, L).

task_1(Sum) :-
    get_input(L),
    maplist(shared_item, L, Vs),
    sumlist(Vs, Sum).

% task_2_helper(+List, -Sum)
task_2_helper([], 0).
task_2_helper([S1, S2, S3|L], Sum) :-
    shared_item_group(S1, S2, S3, V),
    task_2_helper(L, N),
    Sum #= N + V.

task_2(Sum) :-
    get_input(L),
    task_2_helper(L, Sum).
