% nvim: set syntax=prolog

% AoC 2024 Day 14

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- set_prolog_flag(double_quotes, codes).

% :- set_prolog_flag(answer_write_options,[max_depth(0)]).
% :- consult('../helper.pl').

:- initialization(main, main).

% ---------- DCG ----------

line(robot(X-Y, VX-VY)) -->
    "p=", integer(X), ",", integer(Y), " v=", integer(VX), ",", integer(VY).

input(Rs) --> sequence(line, (blank, \+ eos), Rs), blank.

get_robots(Rs) :- phrase_from_file(input(Rs), 'input_14').

% ---------- Part 1 ----------

grid_x(101).
midline_x(50).
grid_y(103).
midline_y(51).

move(T, robot(X-Y, VX-VY), robot(X1-Y1, VX-VY)) :-
    grid_x(MaxX), grid_y(MaxY),
    X1 #= (X + VX * T) mod MaxX,
    Y1 #= (Y + VY * T) mod MaxY, !.

in_quadrant_1(robot(X-Y, _)) :-
    midline_x(MidX), midline_y(MidY),
    X #< MidX, Y #< MidY, !.
in_quadrant_2(robot(X-Y, _)) :-
    midline_x(MidX), midline_y(MidY),
    X #> MidX, Y #< MidY, !.
in_quadrant_3(robot(X-Y, _)) :-
    midline_x(MidX), midline_y(MidY),
    X #< MidX, Y #> MidY, !.
in_quadrant_4(robot(X-Y, _)) :-
    midline_x(MidX), midline_y(MidY),
    X #> MidX, Y #> MidY, !.

task_1(K) :-
    get_robots(Rs),
    maplist(move(100), Rs, NewRs),
    include(in_quadrant_1, NewRs, Q1),
    include(in_quadrant_2, NewRs, Q2),
    include(in_quadrant_3, NewRs, Q3),
    include(in_quadrant_4, NewRs, Q4),
    maplist(length, [Q1, Q2, Q3, Q4], [K1, K2, K3, K4]),
    K #= K1 * K2 * K3 * K4.
