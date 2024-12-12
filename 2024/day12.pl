% nvim: set syntax=prolog

% AoC 2024 Day 12

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- set_prolog_flag(double_quotes, codes).

% :- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- consult('../helper.pl').

:- dynamic visited/2.
:- initialization(main, main).

% ---------- DCG ----------

line(Line) --> sequence(nonblank, Line).

grid(Grid) --> sequence(line, (blank, \+ eos), Grid), blank.

get_grid(Grid) :- phrase_from_file(grid(Grid), 'input_12').

% ---------- Part 1 & 2 ----------

get_elem(Grid, X, Y, Elem) :-
    nth0(X, Grid, Row),
    nth0(Y, Row, Elem).

is_neighbour(Max, X-Y, X-Y1) :-
    (Y1 #= Y + 1; Y1 #= Y - 1),
    Y1 #>= 0, Y1 #< Max.
is_neighbour(Max, X-Y, X1-Y) :-
    (X1 #= X + 1; X1 #= X - 1),
    X1 #>= 0, X1 #< Max.

valid_next_step(Grid, X-Y, X1-Y1) :-
    length(Grid, Max),
    is_neighbour(Max, X-Y, X1-Y1),
    \+ visited(X1, Y1).

route_(Grid, Plant, X-Y, []) :-
    get_elem(Grid, X, Y, Plant2),
    Plant2 #\= Plant, !.
route_(Grid, Plant, X-Y, [X-Y|Route]) :-
    assertz(visited(X, Y)),
    findall(X1-Y1, valid_next_step(Grid, X-Y, X1-Y1), NextSteps),
    maplist(route_(Grid, Plant), NextSteps, Routes),
    foldl(append, Routes, [], Route).
route(Grid, X, Y, Route) :-
    \+ visited(X, Y), !,
    get_elem(Grid, X, Y, Plant),
    route_(Grid, Plant, X-Y, Route).

get_routes_(Grid, X, Routes) :-
    length(Grid, Max),
    range(0, Max, Ys),
    convlist(route(Grid, X), Ys, RoutesWithDup),
    maplist(list_to_set, RoutesWithDup, Routes).
get_routes(Grid, Routes) :-
    length(Grid, Max),
    range(0, Max, Xs),
    convlist(get_routes_(Grid), Xs, NestedRoutes),
    foldl(append, NestedRoutes, [], Routes).

% ---------- Part 1 ----------

fence_count(Grid, X, Y, FenceCount) :-
    get_elem(Grid, X, Y, PlantType),
    XM1 #= X - 1, XP1 #= X + 1, YM1 #= Y - 1, YP1 #= Y + 1,
    exclude({Grid, PlantType}/[X1-Y1]>>get_elem(Grid, X1, Y1, PlantType), [XM1-Y, XP1-Y, X-YM1, X-YP1], Fences),
    length(Fences, FenceCount).

fence_grid_(Grid, X, Fence) :-
    length(Grid, Max),
    range(0, Max, Ys),
    maplist(fence_count(Grid, X), Ys, Fence).
fence_grid(Grid, FenceGrid) :-
    length(Grid, Max),
    range(0, Max, Xs),
    maplist(fence_grid_(Grid), Xs, FenceGrid).

get_perimeter(FenceGrid, Routes, Perimeter) :-
    maplist([X-Y]>>get_elem(FenceGrid, X, Y), Routes, Perimeters),
    sum_list(Perimeters, Perimeter).

% ---------- Part 2 ----------
% Corner can happen only like these:
% .O.   OX.
% OX.   XX.
% ...   ...
% is_corner(+CurrPlant, +(P1, P2, P3))
is_corner(CurrPlant, (P1, P2, _)) :- P1 #\= CurrPlant, P2 #\= CurrPlant, !.
is_corner(CurrPlant, (CurrPlant, CurrPlant, P3)) :- P3 #\= CurrPlant, !.

get_elem_special(_, -1, _, C) :- string_codes(".", [C]), !.
get_elem_special(_, _, -1, C) :- string_codes(".", [C]), !.
get_elem_special(Grid, X, _, C) :-
    length(Grid, Max),
    X #>= Max, string_codes(".", [C]), !.
get_elem_special(Grid, _, Y, C) :-
    length(Grid, Max),
    Y #>= Max, string_codes(".", [C]), !.
get_elem_special(Grid, X, Y, C) :- get_elem(Grid, X, Y, C).

corner_count(Grid, X, Y, CornerCount) :-
    get_elem(Grid, X, Y, CurrentPlant),
    XM1 #= X - 1, XP1 #= X + 1, YM1 #= Y - 1, YP1 #= Y + 1,
    get_elem_special(Grid, XM1, Y, TopPlant),
    get_elem_special(Grid, XP1, Y, BottomPlant),
    get_elem_special(Grid, X, YM1, LeftPlant),
    get_elem_special(Grid, X, YP1, RightPlant),
    get_elem_special(Grid, XM1, YM1, TopLeftPlant),
    get_elem_special(Grid, XM1, YP1, TopRightPlant),
    get_elem_special(Grid, XP1, YM1, BottomLeftPlant),
    get_elem_special(Grid, XP1, YP1, BottomRightPlant),
    include(is_corner(CurrentPlant), [
        (LeftPlant, TopPlant, TopLeftPlant),
        (LeftPlant, BottomPlant, BottomLeftPlant),
        (RightPlant, TopPlant, TopRightPlant),
        (RightPlant, BottomPlant, BottomRightPlant)
    ], Corners),
    length(Corners, CornerCount).

get_corner_count(Grid, Route, CornerCount) :-
    maplist([X-Y]>>corner_count(Grid, X, Y), Route, CornerCounts),
    sum_list(CornerCounts, CornerCount).

% ---------- Main ----------
task_1_aux(FenceGrid, Route, Price) :-
    length(Route, Len),
    get_perimeter(FenceGrid, Route, Perimeter),
    Price #= Len * Perimeter.

task_2_aux(Grid, Route, Price) :-
    length(Route, Len),
    get_corner_count(Grid, Route, CornerCount),
    Price #= Len * CornerCount.

main(_) :-
    get_grid(Grid),
    get_routes(Grid, Routes),
    % Task 1
    fence_grid(Grid, FenceGrid),
    maplist(task_1_aux(FenceGrid), Routes, Prices1),
    sum_list(Prices1, K1),
    format("Task 1: ~d\n", K1),
    % Task 2
    maplist(task_2_aux(Grid), Routes, Prices2),
    sum_list(Prices2, K2),
    format("Task 2: ~d\n", K2).
