% nvim: set syntax=prolog

% AoC 2023 Day 6

:- use_module(library(clpfd)).
:- consult('../helper.pl').

% pre-parsed input
:- consult('input_06').

% strategy(+Time, +Distance, -S)
strategy(Time, Distance, S) :-
    % solve x^2 - tx + d < 0
    % x = (t +- sqrt(t^2 - 4d)) / 2
    % Thank you prolog for your ability of handling floating point numbers
    % Yeah I'm saying this to you ASP >:(
    Delta is Time * Time - 4 * Distance,
    Sqrt is sqrt(Delta),
    X1F is (Time + Sqrt) / 2,
    X1 is floor(X1F),
    X2F is (Time - Sqrt) / 2,
    X2 is ceil(X2F),
    S #= X1 - X2 + 1.

task_1(S) :-
    time_1(Times),
    distance_1(Distances),
    maplist(strategy, Times, Distances, Strategies),
    foldl(mult_, Strategies, 1, S).

task_2(S) :-
    time_2(Time),
    distance_2(Distance),
    strategy(Time, Distance, S).
