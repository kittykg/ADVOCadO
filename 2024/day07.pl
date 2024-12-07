% nvim: set syntax=prolog

% AoC 2024 Day 07

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).
:- consult('../helper.pl').

:- initialization(main, main).

% ---------- DCG ----------
input([]) --> eos, !.
input([(Target, Numbers)|LS]) --> equation(Target, Numbers), input(LS).

equation(Target, Numbers) --> integer(Target), ": ", num_list(Numbers).

num_list([N]) --> integer(N), (eos ; "\n").
num_list([N|NS]) --> integer(N), " ", num_list(NS).

get_equations(Equations) :- phrase_from_file(input(Equations), 'input_07').

% ---------- Solution ----------

compute_(Target, [], _, Acc) :- Target #= Acc, !.
compute_(Target, [N|NS], Flag, Acc) :-
    Acc1 #= Acc + N,
    compute_(Target, NS, Flag, Acc1).
compute_(Target, [N|NS], Flag, Acc) :-
    Acc1 #= Acc * N,
    compute_(Target, NS, Flag, Acc1).
compute_(Target, [N|NS], allow_concat, Acc) :-
    number_string(Acc, B1),
    number_string(N, B2),
    string_concat(B1, B2, B),
    number_string(Acc1, B),
    compute_(Target, NS, allow_concat, Acc1).

task_1(Equations, K) :-
    include([(T, N)]>>compute_(T, N, no_concat, 0), Equations, L),
    maplist(first, L, Targets),
    sum_list(Targets, K).

task_2(Equations, K) :-
    include([(T, N)]>>compute_(T, N, allow_concat, 0), Equations, L),
    maplist(first, L, Targets),
    sum_list(Targets, K).

main(_) :-
    get_equations(Equations),
    task_1(Equations, K1),
    format("Task 1: ~a~n", K1),
    task_2(Equations, K2),
    format("Task 2: ~a~n", K2).
