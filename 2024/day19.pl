% nvim: set syntax=prolog

% AoC 2024 Day 19

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- set_prolog_flag(double_quotes, codes).

:- initialization(main, main).

% ---------- DCG ----------

possible_towels(Ts) --> sequence(string, ", ", Ts).

target_towels(Gs) --> sequence(string, (blank, \+ eos), Gs).

input(Ts, Gs) --> possible_towels(Ts), blank, blank, target_towels(Gs).

get_input(Ts, Gs) :- phrase_from_file(input(Ts, Gs), 'input_19').

% ---------- Part 1 ----------

:- table match/2.

match(_, []) :- !.
match(Ts, G) :-
    append(G1, G2, G),
    member(G2, Ts),
    match(Ts, G1), !.

% ---------- Part 2 ----------

:- table count_match/3.

count_match(_, [], 1).
count_match(Ts, G, Count) :-
    convlist(count_match_(Ts, G), Ts, Cs),
    sum_list(Cs, Count).

count_match_(Ts, G, T, Count) :-
    append(G1, T, G),
    count_match(Ts, G1, Count).

main(_) :-
    get_input(Ts, Gs),
    % Task 1
    include(match(Ts), Gs, Matches),
    length(Matches, K1),
    format("Task 1: ~d\n", K1),
    % Task 2
    maplist(count_match(Ts), Gs, Cs),
    sum_list(Cs, K2),
    format("Task 2: ~d\n", K2).
