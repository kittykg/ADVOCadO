% nvim: set syntax=prolog

% AoC 2022 Day 4

:- consult('../helper.pl').

% split_to_groups(+S, -L1, -L2)
split_to_groups(S, L1, L2) :-
    string_codes(S, L),
    append(L1, [44|L2], L).

% split_to_num_codes(+C, -C1, -C2)
split_to_num_codes(C, C1, C2) :-
    append(C1, [45|C2], C).

% get_groups(+S, -A1, -A2, -B1, -B2)
get_groups(S, A1, A2, B1, B2) :-
    split_to_groups(S, G1, G2), !,
    split_to_num_codes(G1, L1, L2), !,
    split_to_num_codes(G2, L3, L4), !,
    number_codes(A1, L1),
    number_codes(A2, L2),
    number_codes(B1, L3),
    number_codes(B2, L4).

% is_subset(+A1, +A2, +B1, +B2)
is_subset(A1, A2, B1, B2) :-
    A1 #=< B1, B2 #=< A2.
is_subset(A1, A2, B1, B2) :-
    B1 #=< A1, A2 #=< B2.

% has_intersection(+A1, +A2, +B1, +B2)
has_intersection(A1, A2, B1, B2) :-
    is_subset(A1, A2, B1, B2).
has_intersection(A1, A2, B1, B2) :-
    A1 #=< B1, B1 #=< A2, A2 #=< B2.
has_intersection(A1, A2, B1, B2) :-
    B1 #=< A1, A1 #=< B2, B2 #=< A2.

% get_input(-Lines)
get_input(L) :-
    open('input_04', read, Stream),
    read_string_file(Stream, L).

% task_1_helper(+S)
task_1_helper(S) :-
    get_groups(S, A1, A2, B1, B2),
    is_subset(A1, A2, B1, B2).

task_1(Out) :-
    get_input(L),
    include(task_1_helper, L, Satisfied),
    length(Satisfied, Out).

% task_2_helper(+S)
task_2_helper(S) :-
    get_groups(S, A1, A2, B1, B2),
    has_intersection(A1, A2, B1, B2).

task_2(Out) :-
    get_input(L),
    include(task_2_helper, L, Satisfied),
    length(Satisfied, Out).
