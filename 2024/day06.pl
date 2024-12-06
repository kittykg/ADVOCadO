% nvim: set syntax=prolog

% AoC 2024 Day 06

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).
:- consult('../helper.pl').

% ---------- DCG ----------
line([]) --> ("\n"; eos), !.
line([X|XS]) --> [X], line(XS).

grid([]) --> eos, !.
grid([L|LS]) --> line(L), grid(LS).

get_grid(Grid) :- phrase_from_file(grid(Grid), 'input_06').

get_elem(Grid, X, Y, Elem) :-
    nth0(X, Grid, Row),
    nth0(Y, Row, Code),
    string_codes(Elem, [Code]).

start_loc(Grid, X, Y) :- get_elem(Grid, X, Y, "^").

next_step(east, X, Y, X, Y1) :- Y1 #= Y + 1.
next_step(south, X, Y, X1, Y) :- X1 #= X + 1.
next_step(west, X, Y, X, Y1) :- Y1 #= Y - 1.
next_step(north, X, Y, X1, Y) :- X1 #= X - 1.

turn(east, south).
turn(south, west).
turn(west, north).
turn(north, east).

inside(Grid, X, Y) :-
    length(Grid, MaxX),
    Grid =[Row|_],
    length(Row, MaxY),
    X #>= 0, X #< MaxX, Y #>= 0, Y #< MaxY.


go_(Grid, (D, (X, Y)), (D1, (X, Y))) :-
    next_step(D, X, Y, X1, Y1),
    get_elem(Grid, X1, Y1, "#"),
    turn(D, D1), !.
go_(Grid, (D, (X, Y)), (D, (X1, Y1))) :-
    next_step(D, X, Y, X1, Y1),
    inside(Grid, X1, Y1), !.

go(Grid, Curr, [Curr]) :- \+ go_(Grid, Curr, _), !.
go(Grid, Curr, [Curr|Path]) :-
    go_(Grid, Curr, Next),
    go(Grid, Next, Path).

task_1(K) :-
    get_grid(Grid),
    start_loc(Grid, X, Y),
    go(Grid, (north, X, Y), Path),
    maplist([(P)]>>second(P), Path, Locations),
    list_to_set(Locations, UniqueLocations),
    length(UniqueLocations, K).

loop_(_, Curr, History) :- member(Curr, History), !.
loop_(Grid, Curr, History) :-
    go_(Grid, Curr, Next),
    loop_(Grid, Next, [Curr|History]).

change_grid(Grid, NewGrid, (X, Y)) :-
    inside(Grid, X, Y),
    nth0(X, Grid, Row, T1),
    string_codes(".#", [C1, C2]),
    nth0(Y, Row, C1, T2),
    nth0(Y, NewRow, C2, T2),
    nth0(X, NewGrid, NewRow, T1).

task_2_aux(Grid, (CX, CY)) :-
    start_loc(Grid, X, Y),
    change_grid(Grid, NewGrid, (CX, CY)),
    loop_(NewGrid, (north, X, Y), []).

task_2(K) :-
    get_grid(Grid),
    setof((X, Y), Grid^task_2_aux(Grid, (X, Y)), L),
    length(L, K).
