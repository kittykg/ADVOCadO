% nvim: set syntax=prolog

% AoC 2021 Day 7

:- consult('../helper.pl').
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).

%-----------------------------------%
%            IO parsing             %
%-----------------------------------%

% parse_string_to_ints(+String, -ListOfNumbers)
parse_string_to_ints(S, L) :-
    split_string(S, ",", "", LS),
    maplist(atom_number, LS, L).

% get_all_pos(-ListOfPositions)
get_all_pos(L) :-
    open('input_07', read, Stream),
    read_string_file(Stream, [S]), !,
    close(Stream),
    parse_string_to_ints(S, L).

%-----------------------------------%
%           Crab Allignment         %
%-----------------------------------%

% get_range(+ListOfPositions, -Range)
get_range(L, Range) :-
    min_list(L, Min), max_list(L, Max),
    bagof(N, between(Min, Max, N), Range).

% get_fuel_cost_1(+Position, +CrabPosition, -FuelCost)
get_fuel_cost_1(Pos, S, Fuel) :-
    D #= Pos - S,
    abs(D, Fuel).

% get_fuel_cost_2(+Position, +CrabPosition, -FuelCost)
get_fuel_cost_2(Pos, S, Fuel) :-
    D #= Pos - S,
    abs(D, AbsD),
    Fuel is ((1 + AbsD) * AbsD / 2).

% get_total_fuel_1(+Position, +ListOfCrabPosition,
%                  -TotalFuelCost)
get_total_fuel_1(Pos, L, Total) :-
    maplist(Pos+\X^(get_fuel_cost_1(Pos, X)), L, Fuels),
    sumlist(Fuels, Total).

% get_total_fuel_2(+Position, +ListOfCrabPosition,
%                  -TotalFuelCost)
get_total_fuel_2(Pos, L, Total) :-
    maplist(Pos+\X^(get_fuel_cost_2(Pos, X)), L, Fuels),
    sumlist(Fuels, Total).

% get_min_fuel_1(+ListOfCrabPositions, -MinFuelCost)
get_min_fuel_1(L, Min) :-
    get_range(L, Range),
    maplist(L+\R^(get_total_fuel_1(R, L)), Range, AllFuels),
    min_list(AllFuels, Min).

% get_min_fuel_2(+ListOfCrabPositions, -MinFuelCost)
get_min_fuel_2(L, Min) :-
    get_range(L, Range),
    maplist(L+\R^(get_total_fuel_1(R, L)), Range, AllFuels),
    min_list(AllFuels, Min).

task_1(Out) :-
    get_all_pos(L), !,
    get_min_fuel_1(L, Out).

task_2(Out) :-
    get_all_pos(L), !,
    get_min_fuel_2(L, Out).
