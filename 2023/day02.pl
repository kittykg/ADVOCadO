% nvim: set syntax=prolog

% AoC 2023 Day 2

:- use_module(library(clpfd)).
:- consult('../helper.pl').

% pre-parsed input
:- consult('input_02').

% is_colour(+Colour, +(Colour, _)).
is_colour(Colour, (Colour, _)).

% get_colour_min_count(+List, +Colour, -Count)
get_colour_max_count(List, Colour, Count) :-
    include(is_colour(Colour), List, CL),
    maplist(second, CL, Counts),
    max_list(Counts, Count).

% satisfy(+(_, L))
satisfy((_, L)) :-
    % 12 red cubes, 13 green cubes, 14 blue cubes
    get_colour_max_count(L, red, RM),
    RM #=< 12,
    get_colour_max_count(L, green, GM),
    GM #=< 13,
    get_colour_max_count(L, blue, BM),
    BM #=< 14.

% power(+(_, L), -Power)
power((_, L), Power) :-
    get_colour_max_count(L, red, RM),
    get_colour_max_count(L, green, GM),
    get_colour_max_count(L, blue, BM),
    Power #= RM * GM * BM.

task_1(Sum) :-
    input(L),
    include(satisfy, L, SL),
    maplist(first, SL, GameIds),
    sum_list(GameIds, Sum).

task_2(Power) :-
    input(L),
    maplist(power, L, Powers),
    sum_list(Powers, Power).
