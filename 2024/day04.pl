% nvim: set syntax=prolog

% AoC 2024 Day 4

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

% ---------- DCG ----------
line([]) --> ("\n"; eos), !.
line([X|XS]) --> [X], line(XS).

grid([]) --> eos, !.
grid([L|LS]) --> line(L), grid(LS).

get_grid(Grid) :- phrase_from_file(grid(Grid), 'input_04').

% ---------- Utils ----------
get_elem(Grid, X, Y, Elem) :-
    nth0(X, Grid, Row),
    nth0(Y, Row, Elem).

get_max_x_y(Grid, MaxX, MaxY) :-
    length(Grid, MaxX),
    Grid = [Row|_],
    length(Row, MaxY).

% ---------- Part 1 ----------

% Go vertical
xmas(Grid, [(X, Y), (X, Y1), (X, Y2), (X, Y3)]) :-
    get_max_x_y(Grid, MaxX, MaxY),
    X #>= 0, X #< MaxX, Y #>= 0, Y #< MaxY,
    string_codes("XMAS", [C, C1, C2, C3]),
    get_elem(Grid, X, Y, C),
    abs(Y1 - Y) #= 1, abs(Y2 - Y1) #= 1, abs(Y3 - Y2) #= 1,
    ( chain([Y, Y1, Y2, Y3], #>) ; chain([Y, Y1, Y2, Y3], #<) ),
    get_elem(Grid, X, Y1, C1), get_elem(Grid, X, Y2, C2), get_elem(Grid, X, Y3, C3).

% Go horizontal
xmas(Grid, [(X, Y), (X1, Y), (X2, Y), (X3, Y)]) :-
    get_max_x_y(Grid, MaxX, MaxY),
    X #>= 0, X #< MaxX, Y #>= 0, Y #< MaxY,
    string_codes("XMAS", [C, C1, C2, C3]),
    get_elem(Grid, X, Y, C),
    abs(X1 - X) #= 1, abs(X2 - X1) #= 1, abs(X3 - X2) #= 1,
    ( chain([X, X1, X2, X3], #>) ; chain([X, X1, X2, X3], #<) ),
    get_elem(Grid, X1, Y, C1), get_elem(Grid, X2, Y, C2), get_elem(Grid, X3, Y, C3).

% Go diagonal
xmas(Grid, [(X, Y), (X1, Y1), (X2, Y2), (X3, Y3)]) :-
    get_max_x_y(Grid, MaxX, MaxY),
    X #>= 0, X #< MaxX, Y #>= 0, Y #< MaxY,
    string_codes("XMAS", [C, C1, C2, C3]),
    get_elem(Grid, X, Y, C),
    abs(X1 - X) #= 1, abs(X2 - X1) #= 1, abs(X3 - X2) #= 1,
    ( chain([X, X1, X2, X3], #>) ; chain([X, X1, X2, X3], #<) ),
    abs(Y1 - Y) #= 1, abs(Y2 - Y1) #= 1, abs(Y3 - Y2) #= 1,
    ( chain([Y, Y1, Y2, Y3], #>) ; chain([Y, Y1, Y2, Y3], #<) ),
    get_elem(Grid, X1, Y1, C1), get_elem(Grid, X2, Y2, C2), get_elem(Grid, X3, Y3, C3).


task_1(K) :-
    get_grid(Grid),
    findall(L, xmas(Grid, L), B),
    length(B, K).

% ---------- Part 2 ----------

check_mas(E1, E, E2) :- string_codes("MAS", [E1, E, E2]).
check_mas(E1, E, E2) :- string_codes("MAS", [E2, E, E1]).

x_shape_mas(Grid, [(X1, Y1), (X1, Y2), (X, Y), (X2, Y1), (X2, Y2)]) :-
    get_max_x_y(Grid, MaxX, MaxY),
    X #>= 0, X #< MaxX, Y #>= 0, Y #< MaxY,
    X1 #= X - 1, X2 #= X + 1, Y1 #= Y - 1, Y2 #= Y + 1,
    get_elem(Grid, X, Y, E),
    get_elem(Grid, X1, Y1, E1), get_elem(Grid, X2, Y2, E2), check_mas(E1, E, E2),
    get_elem(Grid, X1, Y2, E3), get_elem(Grid, X2, Y1, E4), check_mas(E3, E, E4).

task_2(K) :-
    get_grid(Grid),
    findall(L, x_shape_mas(Grid, L), B),
    length(B, K).
