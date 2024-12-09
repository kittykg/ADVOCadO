% nvim: set syntax=prolog

% AoC 2024 Day 08

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- set_prolog_flag(double_quotes, codes).
% :- set_prolog_flag(answer_write_options,[max_depth(0)]).

:- dynamic antenna/3.
:- initialization(main, main).

% ---------- DCG ----------

cell(empty) --> ".", !.
cell(C) --> nonblank(C).

line(Line) --> sequence(cell, Line).

grid(Grid) --> sequence(line, (blank, \+ eos), Grid), blank.

get_grid(Grid) :- phrase_from_file(grid(Grid), 'input_08').

% ---------- Part 1 & 2 ----------

get_antennas_([], _, _).
get_antennas_([E|Es], X, Y) :-
    (E == empty ->
        true;
        assertz(antenna(E, X, Y))
    ),
    Y1 #= Y + 1,
    get_antennas_(Es, X, Y1).

get_antennas([], _).
get_antennas([R|Rs], X) :-
    get_antennas_(R, X, 0),
    X1 #= X + 1,
    get_antennas(Rs, X1).

in_bounds(Bound, X, Y) :-
    X #>= 0, X #< Bound,
    Y #>= 0, Y #< Bound.

% for task 1
antinode(Bound, NX, NY) :-
    antenna(E, X1, Y1), antenna(E, X2, Y2),
    X1 #\= X2, Y1 #\= Y2,
    DX #= X2 - X1, DY #= Y2 - Y1,
    (
        NX #= X1 - DX, NY #= Y1 - DY;
        NX #= X2 + DX, NY #= Y2 + DY
    ),
    in_bounds(Bound, NX, NY).

% for task 2
antinode_2(Bound, NX, NY) :-
    antenna(E, X1, Y1), antenna(E, X2, Y2),
    X1 #\= X2, Y1 #\= Y2,
    DX #= X2 - X1, DY #= Y2 - Y1,
    NegBound #= -Bound,
    between(NegBound, Bound, M),
    NX #= X2 + DX * M, NY #= Y2 + DY * M,
    in_bounds(Bound, NX, NY).

% ---------- Main ----------
main(_) :-
    get_grid(Grid),
    length(Grid, Bound),
    get_antennas(Grid, 0),
    % task 1
    setof((NX, NY), antinode(Bound, NX, NY), S1), length(S1, K1),
    format("Task 1: ~a~n", K1),
    % task 2
    setof((NX, NY), antinode_2(Bound, NX, NY), S2), length(S2, K2),
    format("Task 2: ~a~n", K2).
