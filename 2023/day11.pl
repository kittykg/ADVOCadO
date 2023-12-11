% nvim: set syntax=prolog

% AoC 2023 Day 11

:- use_module(library(clpfd)).
:- consult('../helper.pl').

% pre-parsed input.
:- consult('input_11').


% expanding_rows(-EXR)
expanding_rows(EXR) :-
    bagof(X, Y^coord(X, Y), Rows),
    list_to_set(Rows, RowSet),
    x_len(XLen),
    range(0, XLen, R),
    list_to_set(R, FullRow),
    subtract(FullRow, RowSet, EXR).

% expanding_cols(-EXC)
expanding_cols(EXC) :-
    bagof(Y, X^coord(X, Y), Cols),
    list_to_set(Cols, ColSet),
    y_len(YLen),
    range(0, YLen, C),
    list_to_set(C, FullCol),
    subtract(FullCol, ColSet, EXC).

expand_coord_(EXSet, C, Distance, NewC) :-
    include(>(C), EXSet, L),
    length(L, Len),
    NewC #= C + Len * Distance.

% expand_coord(+Distance, +Coord, -NewCoord)
expand_coord(Distance, (X, Y), (NewX, NewY)) :-
    expanding_rows(EXR),
    expanding_cols(EXC),
    expand_coord_(EXR, X, Distance, NewX),
    expand_coord_(EXC, Y, Distance, NewY).

% expand(+Distance, -NewCoords)
expand(Distance, NewCoords) :-
    bagof((X, Y), X^Y^coord(X, Y), OGCoords),
    maplist(expand_coord(Distance), OGCoords, NewCoords).

% manhattan_distance(+Coord1, +Coord2, -Distance)
manhattan_distance((X1, Y1), (X2, Y2), D) :-
    D #= abs(X1 - X2) + abs(Y1 - Y2).

get_pair_distance_([], _, 0).
get_pair_distance_([C1|H], C1, AccSum) :-
    get_pair_distance_(C1, H, AccSum).
get_pair_distance_([C2|H], C1, AccSum) :-
    manhattan_distance(C1, C2, D),
    get_pair_distance_(H, C1, AccSum1),
    AccSum #= AccSum1 + D.

% task(+Distance, -AccSum)
task(Distance, AccSum) :-
    expand(Distance, NewCoords),
    maplist(get_pair_distance_(NewCoords), NewCoords, PD),
    sum_list(PD, AS),
    AccSum #= AS // 2.

task_1(AccSum) :-
    task(1, AccSum).
task_2(AccSum) :-
    I #= 1000000 - 1, task(I, AccSum).
