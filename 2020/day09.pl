% nvim :set syntax=prolog

% AoC 2020 Day 9

:- consult('helper.pl').

find_pair([X|L], Goal, X, Target) :-
    Target is Goal - X, member(Target, L).
find_pair([_, X|L], Goal, V1, V2) :- find_pair([X|L], Goal, V1, V2).

check(PreambleLen, L, Goal) :-
    append(Pre, _, L),
    length(Pre, PreambleLen),
    find_pair(Pre, Goal, _, _).

task_1_([X|L], N) :-
    PreambleLen is 25,
    nth0(PreambleLen, [X|L], Goal),
    ( check(PreambleLen, [X|L], Goal) ->
        task_1_(L, N) ;
        N is Goal
    ).

task_1(N) :-
    open('input_09', read, Stream),
    read_number_file(Stream, L), !,
    close(Stream),
    task_1_(L, N).

min_plus_max(L, Res) :-
    min_member(Min, L), max_member(Max, L), Res is Min + Max.

task_2_(CurrWinStart, CurrWinEnd, L, Goal, Res) :-
    sub_list(L, CurrWinStart, CurrWinEnd, Sub),
    sum_list(Sub, Sum),
    ( Sum == Goal ->
        min_plus_max(Sub, Res);
        ( Sum > Goal ->
            NextWinStart is CurrWinStart + 1,
            NextWinEnd is CurrWinEnd ;
            NextWinStart is CurrWinStart,
            NextWinEnd is CurrWinEnd + 1
        ),
        task_2_(NextWinStart, NextWinEnd, L, Goal, Res)
    ).

task_2(N) :-
    open('input_09', read, Stream),
    read_number_file(Stream, L), !,
    close(Stream),
    task_1_(L, Goal),
    task_2_(0, 1, L, Goal, N).
