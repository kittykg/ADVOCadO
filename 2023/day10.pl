% nvim: set syntax=prolog

% AoC 2023 Day 10

:- use_module(library(clpfd)).
:- consult('../helper.pl').

% pre-parsed input.
:- consult('input_10').


% in_bounds(+X, +Y)
in_bounds((X, Y)) :-
    x_bound(XMin, XMax),
    y_bound(YMin, YMax),
    X #>= XMin,
    X #=< XMax,
    Y #>= YMin,
    Y #=< YMax, !.

% connection(+Coord, -List)
connection(Coord, []) :-
    start_coord(Coord).
connection((X, Y), L) :-
    pipe((X, Y), '|'),
    X1 #= X - 1,
    X2 #= X + 1,
    include(in_bounds, [(X1, Y), (X2, Y)], L).
connection((X, Y), L) :-
    pipe((X, Y), '-'),
    Y1 #= Y - 1,
    Y2 #= Y + 1,
    include(in_bounds, [(X, Y1), (X, Y2)], L).
connection((X, Y), L) :-
    pipe((X, Y), 'L'),
    X1 #= X - 1,
    Y1 #= Y + 1,
    include(in_bounds, [(X1, Y), (X, Y1)], L).
connection((X, Y), L) :-
    pipe((X, Y), 'J'),
    X1 #= X - 1,
    Y1 #= Y - 1,
    include(in_bounds, [(X1, Y), (X, Y1)], L).
connection((X, Y), L) :-
    pipe((X, Y), '7'),
    X1 #= X + 1,
    Y1 #= Y - 1,
    include(in_bounds, [(X1, Y), (X, Y1)], L).
connection((X, Y), L) :-
    pipe((X, Y), 'F'),
    X1 #= X + 1,
    Y1 #= Y + 1,
    include(in_bounds, [(X1, Y), (X, Y1)], L).

% connected_with_pipe(+Coord, +Coord)
connected_with_pipe(X, Y) :-
    start_coord(X),
    connection(Y, YPL),
    member(X, YPL).
connected_with_pipe(X, Y) :-
    start_coord(Y),
    connection(X, XPL),
    member(Y, XPL).
cconnected_with_pipe(X, Y) :-
    connection(X, XPL),
    connection(Y, YPL),
    member(Y, XPL),
    member(X, YPL).


% next_coord(+Coord, +Coord, -Coord)
next_coord(X, ComeFrom, Next) :-
    connection(X, XPL),
    include(\=(ComeFrom), XPL, [Next]), !.

% loop_start_candidate(-C)
loop_start_candidate(C) :-
    start_coord(S),
    pipe(C, _),
    connected_with_pipe(S, C).

% loop_start_candidates(-C1, -C2)
loop_start_candidates(C1, C2) :-
    bagof(C, loop_start_candidate(C), [C1, C2]).

loop_(LC, _, LC, _, [], []).
loop_(LC1, LCCF1, LC2, LCCF2, [LC1N | L1N], [LC2N | L2N]) :-
    next_coord(LC1, LCCF1, LC1N),
    next_coord(LC2, LCCF2, LC2N),
    loop_(LC1N, LC1, LC2N, LC2, L1N, L2N).

% loop(-Loop1, -Loop2)
loop([S, C1|L1], [S, C2|L2]) :-
    start_coord(S),
    loop_start_candidates(C1, C2), !,
    loop_(C1, S, C2, S, L1, L2).

task_1(Res) :-
    loop(L1, L2), !,
    length(L1, L1L),
    length(L2, L2L),
    Res #= max(L1L, L2L) - 1.

% full_loop(-Loop)
full_loop(L) :-
    loop(L1, [_|L2]), !,
    reverse(L2, [_|L2R]),
    append(L1, L2R, L).

% det(+Coord, +Coord, -Det)
det((C1, C2), (C3, C4), Det) :-
    Det #= C1 * C4 - C2 * C3.

% shoelace_area(+FullLoop, -Area)
shoelace_area([H|T], Area) :-
    append(T, [H], L),
    maplist(det, [H|T], L, DetSum),
    sum_list(DetSum, Sum),
    Area #= abs(Sum) // 2.

task_2(Res) :-
    full_loop(FullLoop),
    shoelace_area(FullLoop, Area),
    length(FullLoop, Len),
    Res #= Area - Len div 2 + 1.
