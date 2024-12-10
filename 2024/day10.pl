% nvim: set syntax=prolog

% AoC 2024 Day 10

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- set_prolog_flag(double_quotes, codes).
% :- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- consult('../helper.pl').

:- dynamic start_end/2.
:- initialization(main, main).

% ---------- DCG ----------

line(Line) --> sequence(digit, Line).

grid(Grid) --> sequence(line, (blank, \+ eos), Grid), blank.

get_grid(Grid) :- phrase_from_file(grid(Grid), 'input_10').

% ---------- Part 1 & 2 ----------
% Notes: didn't convert char codes to int, so grid's elements are ASCII codes
% 48 is the ASCII code for '0', 57 is the ASCII code for '9'

get_elem(Grid, X, Y, Elem) :-
    nth0(X, Grid, Row),
    nth0(Y, Row, Elem).

starting_loc(Grid, X-Y) :- get_elem(Grid, X, Y, 48).

valid_next_step(Grid, X1-Y1, X2-Y2) :-
    abs(X1 - X2) + abs(Y1 - Y2) #= 1,
    get_elem(Grid, X1, Y1, E1),
    get_elem(Grid, X2, Y2, E2),
    E2 - E1 #= 1.

route_(Grid, X-Y, [X-Y]) :- get_elem(Grid, X, Y, 57), !.
route_(Grid, X-Y, [X-Y|Route]) :-
    valid_next_step(Grid, X-Y, X1-Y1),
    route_(Grid, X1-Y1, Route).

route(Grid, Route) :-
    starting_loc(Grid, Start),
    route_(Grid, Start, Route).

get_start_end([Start|Rest]) :-
    last(Rest, End),
    assertz(start_end(Start, End)).

task_1_aux(K) :-
    setof(End, start_end(_, End), Ends),
    length(Ends, K).

task_1(Routes, K) :-
    maplist(get_start_end, Routes),
    bagof(K, task_1_aux(K), Ks),
    sumlist(Ks, K).

main(_) :-
    get_grid(Grid),
    bagof(Route, route(Grid, Route), Routes), !,
    % task 1
    task_1(Routes, K1),
    format("Task 1: ~d\n", K1),
    % task 2
    length(Routes, K2),
    format("Task 2: ~d\n", K2).
