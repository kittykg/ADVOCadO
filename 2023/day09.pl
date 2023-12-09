% nvim: set syntax=prolog

% AoC 2023 Day 9

:- use_module(library(clpfd)).
:- consult('../helper.pl').

% pre-parsed input
:- consult('input_09').


% sequence_diff(+Sequence, -DiffSequence)
sequence_diff([_], []).
sequence_diff([A, B | T], [D | DT]) :-
    D #= B - A,
    sequence_diff([B | T], DT).

% sequence_diff_r(+Sequence, -DiffSequences)
sequence_diff_r(Sequence, [Diff]) :-
    sequence_diff(Sequence, Diff),
    list_to_set(Diff, [0]).
sequence_diff_r(Sequence, [Diff | OtherDiffs]) :-
    sequence_diff(Sequence, Diff),
    sequence_diff_r(Diff, OtherDiffs).

% get_final_extrapolate(+ExtrapolateFn, +NumberList, -V)
get_final_extrapolate(ExtrapolateFn, NumberList, V) :-
    sequence_diff_r(NumberList, Diffs),
    reverse([NumberList | Diffs], Pyramid),
    foldl(ExtrapolateFn, Pyramid, 0, V).

% extrapolate_1(+Sequence, +Diff, -V)
extrapolate_1(Sequence, Diff, V) :-
    last(Sequence, Last),
    V #= Last + Diff.

% extrapolate_2(+Sequence, +Diff, -V)
extrapolate_2([H|_], Diff, V) :- V #= H - Diff.

task_1(VSum) :-
    input(NumberLists),
    maplist(get_final_extrapolate(extrapolate_1), NumberLists, Vs),
    sum_list(Vs, VSum).

task_2(VSum) :-
    input(NumberLists),
    maplist(get_final_extrapolate(extrapolate_2), NumberLists, Vs),
    sum_list(Vs, VSum).
