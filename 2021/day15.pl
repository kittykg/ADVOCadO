% nvim: set syntax=prolog

% AoC 2021 Day 15

:- consult('../helper.pl').
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).

% Board: [[Int]]

%-----------------------------------%
%            IO parsing             %
%-----------------------------------%

% parse_string_to_ints(+String, -Ints)
parse_string_to_ints(S, L) :-
    string_chars(S, Chars),
    maplist(atom_number, Chars, L).
