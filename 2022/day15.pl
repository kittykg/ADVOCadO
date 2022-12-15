% nvim: set syntax=prolog

% AoC 2022 Day 15

:- consult('../helper').

% day15_input.pl should contain 2 predicates which return parsed lists:
% get_input(-List) and get_example_input(-L).
:- consult('day15_input').

% Need bit more stack space
:- set_prolog_flag(stack_limit, 2_147_483_648).

manhattan_distance((X1, Y1), (X2, Y2), Dist) :-
    abs(X1 - X2, D1),
    abs(Y1 - Y2, D2),
    Dist #= D1 + D2.

% ---------------- Task 1 ----------------
covered_x(Y, (_, SY), Dist, []) :-
    abs(Y - SY, Y_Diff),
    Y_Diff #> Dist.
covered_x(Y, (SX, SY), Dist, Res) :-
    abs(Y - SY, Y_Diff),
    Start #= SX - Dist + Y_Diff,
    End #= SX + Dist - Y_Diff + 1,
    range(Start, End, Res).

task_1(TargetY, Len) :-
    get_input(L),
    unzip(L, Sensors, Beacons),
    include(on_line(TargetY), Sensors, S),
    unzip(S, SX, _),
    list_to_set(SX, SXSet),
    include(on_line(TargetY), Beacons, B),
    unzip(B, BX, _),
    list_to_set(BX, BXSet), !,
    task_1_(L, TargetY, CoveredX),
    list_to_set(CoveredX, CXSet),
    exclude(is_sensor_or_beacon(SXSet, BXSet), CXSet, ResX),
    length(ResX, Len).

on_line(TargetY, (_, TargetY)).
is_sensor_or_beacon(SX, BX, X) :-
    member(X, SX), ! ; member(X, BX), !.

task_1_([], _, []).
task_1_([(S, B)|Lines], TargetY, Out) :-
    manhattan_distance(S, B, Dist),
    covered_x(TargetY, S, Dist, Covered),
    task_1_(Lines, TargetY, Res),
    append(Covered, Res, Out).

% ---------------- Task 2 ----------------

boundary_bs((SX, SY), Dist, [B1, B2], [B3, B4]) :-
    B1 #= SY - SX + Dist + 1,
    B2 #= SY - SX - Dist - 1,
    B3 #= SY + SX - Dist - 1,
    B4 #= SY + SX + Dist + 1.

line_intersection(B1, B2, (X, Y)) :-
    X #= (B2 - B1) // 2,
    Y #= (B2 + B1) // 2.

in_bound(Bound, (X, Y)) :-
    X #>= 0, X #< Bound,
    Y #>= 0, Y #< Bound.

task_2_prep_([], [], [], []).
task_2_prep_([(S, B)|Lines], M1BOut, MM1BOut, [(S, Dist)|STRes]) :-
    manhattan_distance(S, B, Dist),
    boundary_bs(S, Dist, M1B, MM1B),
    task_2_prep_(Lines, M1BRes, MM1BRes, STRes),
    append(M1B, M1BRes, M1BOut),
    append(MM1B, MM1BRes, MM1BOut).


task_2(Bound, Product) :-
    get_input(L),
    task_2_prep_(L, M1B, MM1B, ST),
    task_2_(M1B, MM1B, ST, Bound, sat, Product).

task_2_([B1|_], MM1B, ST, Bound, sat, Product) :-
    task_2__(B1, MM1B, ST, Bound, sat, Product).
task_2_([_|M1B], MM1B, ST, Bound, Res, Product) :-
    task_2_(M1B, MM1B, ST, Bound, Res, Product).

task_2__(_, [], _, _, unsat, -1).
task_2__(B1, [B2|_], ST, Bound, sat, Product) :-
    task_2_inner_(B1, B2, ST, Bound, sat, Product).
task_2__(B1, [B2|MM1B], ST, Bound, Res, Product) :-
    task_2_inner_(B1, B2, ST, Bound, unsat, -1),
    task_2__(B1, MM1B, ST, Bound, Res, Product).

task_2_inner_(B1, B2, _, Bound, unsat, -1) :-
    line_intersection(B1, B2, (IX, IY)),
    \+ in_bound(Bound, (IX, IY)).
task_2_inner_(B1, B2, ST, Bound, unsat, -1) :-
    line_intersection(B1, B2, (IX, IY)),
    in_bound(Bound, (IX, IY)),
    task_2_inner__((IX, IY), ST, unsat).
task_2_inner_(B1, B2, ST, Bound, sat, Product) :-
    line_intersection(B1, B2, (IX, IY)),
    in_bound(Bound, (IX, IY)),
    task_2_inner__((IX, IY), ST, O),
    O = sat,
    Product #= IX * 4000000 + IY.


task_2_inner__(_, [], sat).
task_2_inner__((IX, IY), [(S, D)|_], unsat) :-
    manhattan_distance((IX, IY), S, Dist),
    Dist #=< D, !.
task_2_inner__((IX, IY), [_|Rest], Out) :-
    task_2_inner__((IX, IY), Rest, Out).
