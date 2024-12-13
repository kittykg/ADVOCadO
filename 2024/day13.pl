% nvim: set syntax=prolog

% AoC 2024 Day 13

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- set_prolog_flag(double_quotes, codes).

:- initialization(main, main).

% ---------- DCG ----------

claw_machine(claw(AX-AY, BX-BY, TX-TY)) -->
    "Button A: X+", integer(AX), ", Y+", integer(AY), "\n",
    "Button B: X+", integer(BX), ", Y+", integer(BY), "\n",
    "Prize: X=", integer(TX), ", Y=", integer(TY).

input(Claws) --> sequence(claw_machine, (blank, blank), Claws), blank.

get_claw_machines(Claws) :- phrase_from_file(input(Claws), 'input_13').

% ---------- Part 1 ----------

get_prize(Part, claw(AX-AY, BX-BY, TX-TY), MA, MB) :-
    (Part = p2 -> Diff = 10000000000000; Diff = 0),
    MA in 0 .. sup, MB in 0 .. sup,
    Diff + TX #= AX * MA + BX * MB,
    Diff + TY #= AY * MA + BY * MB,
    indomain(MA), indomain(MB).


get_min_token(Part, C, Token) :-
    get_prize(Part, C, MA, MB), !,
    Token #= MA * 3 + MB.

task(Part, Claws, K) :-
    convlist(get_min_token(Part), Claws, Tokens),
    sumlist(Tokens, K).

main(_) :-
    get_claw_machines(Claws),
    % Task 1
    task(p1, Claws, K1),
    format("Task 1: ~d\n", K1),
    % Task 2
    task(p2, Claws, K2),
    format("Task 2: ~d\n", K2).
