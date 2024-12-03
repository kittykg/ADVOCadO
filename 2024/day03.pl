% nvim: set syntax=prolog

% AoC 2024 Day 3

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

% ---------- DCG ----------
mul_instruction(A, B) -->
    "mul(", integer(A), ",", integer(B), ")".
input([]) --> eos, !.
input([mul(A,B)|Is]) -->
    mul_instruction(A,B), input(Is).
% Part 2: handle toggle
input([dont|Is]) -->
    "don't()", input(Is).
input([do|Is]) -->
    "do()", input(Is).
% Ignore gibberish
input(I) --> [_], input(I).

% ---------- Solution ----------

% accumulate(+Instruction, +V0, -V1)
accumulate(mul(A, B), V0, V1) :- V1 #= A * B + V0.
accumulate(_, V, V).

task_1(K) :-
    phrase_from_file(input(Input), 'input_03'),
    foldl(accumulate, Input, 0, K).

% accumulate_with_toggle(+Instructions, +Toggle, -Sum)
accumulate_with_toggle([], _, 0).
accumulate_with_toggle([mul(A,B)|Is], on, Sum) :-
    accumulate_with_toggle(Is, on, Sum1),
    Sum #= A * B + Sum1.
accumulate_with_toggle([dont|Is], _, Sum) :-
    accumulate_with_toggle(Is, off, Sum).
accumulate_with_toggle([do|Is], _, Sum) :-
    accumulate_with_toggle(Is, on, Sum).
accumulate_with_toggle([_|Is], off, Sum) :-
    accumulate_with_toggle(Is, off, Sum).

task_2(K) :-
    phrase_from_file(input(Input), 'input_03'),
    accumulate_with_toggle(Input, on, K).
