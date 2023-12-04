% nvim: set syntax=prolog

% AoC 2023 Day 4

:- use_module(library(clpfd)).
:- consult('../helper.pl').

% pre-parsed input
:- consult('input_04').

% point(+(_, (WN, MN)), -Point)
point((_, (WN, MN)), 0) :-
    intersection(WN, MN, Intersection),
    length(Intersection, 0).
point((_, (WN, MN)), Point) :-
    intersection(WN, MN, Intersection),
    length(Intersection, L),
    Exp #= L - 1,
    power(2, Exp, Point).

task_1(Sum) :-
    input(L),
    maplist(point, L, Points),
    sumlist(Points, Sum).

% win_count(+(_, (WN, MN)), -Count)
win_count((_, (WN, MN)), Count) :-
    intersection(WN, MN, Intersection),
    length(Intersection, Count).

% all_win_counts(-WinCounts)
all_win_counts(WinCounts) :-
    input(L),
    maplist(win_count, L, WinCounts).

% accumulate_cards(+Base, +WinCounts, -Cards)
accumulate_cards([], [], []).
accumulate_cards([Base|BL], [0|WCL], [Base|CL]) :-
    accumulate_cards(BL, WCL, CL).
accumulate_cards([Base|BL], [WinCounts|WCL], [Base|CL]) :-
    append(Won, Rest, BL),
    length(Won, WinCounts),
    maplist(add(Base), Won, NewWon),
    append(NewWon, Rest, NewBL),
    accumulate_cards(NewBL, WCL, CL).

task_2(Sum) :-
    input(L),
    all_win_counts(WinCounts),
    length(L, Len),
    repeat(1, Len, Base),
    accumulate_cards(Base, WinCounts, Cards),
    sumlist(Cards, Sum).