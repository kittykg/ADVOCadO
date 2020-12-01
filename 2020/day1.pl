% nvim: set syntax=prolog

% AoC 2020 Day 1

:- use_module(library(lists)).

:- consult('helper.pl').

sat_sum(V1, V2, Goal) :- (V1 + V2) =:= Goal.

% find_pair(+List, +Goal, -Found, -Value1, -Value2)
find_pair(L, _, false, _, _) :-
    is_list(L), length(L, N), N < 2.
find_pair([X|L], Goal, true, X, Y) :-
    member(Y, L), sat_sum(X, Y, Goal).
find_pair([_, X|L], Goal, Found, V1, V2) :-
    find_pair([X|L], Goal, Found, V1, V2).

task_1(Prod) :-
    open('input_01', read, Stream),
    read_number_file(Stream, N),!,
    close(Stream),
    is_list(N), find_pair(N, 2020, true, V1, V2),!,
    Prod is V1 * V2.

% find_triple(+List, +Goal, -Found, -Values)
find_triple(L, _, false, _) :-
    is_list(L), length(L, N), N < 3.
find_triple([X|L], Goal, true, [X, V1, V2]) :-
    Subgoal is (Goal - X),
    find_pair(L, Subgoal, true, V1, V2).
find_triple([_, X|L], Goal, Found, Vs) :-
    find_triple([X|L], Goal, Found, Vs).

task_2(Prod) :-
    open('input_01', read, Stream),
    read_number_file(Stream, N),!,
    close(Stream),
    is_list(N), find_triple(N, 2020, true, [V1, V2, V3]),!,
    Prod is V1 * V2 * V3.
