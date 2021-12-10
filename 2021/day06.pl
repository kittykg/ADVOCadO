% nvim: set syntax=prolog

% AoC 2021 Day 6

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

%-----------------------------------%
%           Fish Counting           %
%-----------------------------------%

% get_initial_fish(-CountList)
get_inital_fish(CL) :-
    open('input_06', read, Stream),
    read_string_file(Stream, [S]), !,
    close(Stream),
    parse_string_to_ints(S, L),
    bagof(N, between(0, 8, N), XList),
    maplist(L+\E^(count(E, L)), XList, CL).

% progress_one_day(+CountList, -NewCountList)
progress_one_day([N0|R], NewL) :-
    split_at(6, R, N1to6, [N7, N8]),
    NN6 #= N7 + N0,
    append(N1to6, [NN6, N8, N0], NewL).

% progress(+CountList, +DaysLeft, -NewCountList)
progress(OL, 0, OL).
progress(OL, Days, NL) :-
    progress_one_day(OL, NewL),
    LeftDays #= Days - 1,
    progress(NewL, LeftDays, NL).

task(Days, Out) :-
    get_inital_fish(CL),
    progress(CL, Days, NL),
    sumlist(NL, Out).
