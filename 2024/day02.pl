% nvim: set syntax=prolog

% AoC 2024 Day 2

:- use_module(library(clpfd)).
:- consult('../helper.pl').

% get_input(-Lines)
get_input(L) :-
    open('input_02', read, Stream),
    read_string_file(Stream, L).

% get_level_from_line(+Line, -Level)
get_level_from_line(Line, Level) :-
    split_string(Line, " ", "", SL),
    maplist(atom_number, SL, Level).

% get_full_level(-FullLevel)
get_full_level(FullLevel) :-
    get_input(L),
    maplist(get_level_from_line, L, FullLevel).

% safe(+Level)
safe([N1, N2|L]) :-
    N1 #< N2,
    abs(N2 - N1) #=< 3,
    safe_([N2|L], inc).
safe([N1, N2|L]) :-
    N1 #> N2,
    abs(N2 - N1) #=< 3,
    safe_([N2|L], dec).

safe_([_], _).
safe_([N1, N2|L], inc) :-
    N1 #< N2,
    abs(N2 - N1) #=< 3,
    safe_([N2|L], inc).
safe_([N1, N2|L], dec) :-
    N1 #> N2,
    abs(N2 - N1) #=< 3,
    safe_([N2|L], dec).

task_1(K) :-
    get_full_level(FullLevel),
    include(safe, FullLevel, SafeLevels),
    length(SafeLevels, K).

% safe_2(+Level)
safe_2(Level) :- safe(Level).
safe_2(Level) :-
    safe_2_(Level, 0).

safe_2_(Level, Index) :-
    length(Level, LevelLen),
    Index #> LevelLen, !,
    false.
safe_2_(Level, Index) :-
    length(L1, Index),
    append(L1, [_|L2], Level),
    append(L1, L2, NewLevel),
    safe(NewLevel).
safe_2_(Level, Index) :-
    Index1 #= Index + 1,
    safe_2_(Level, Index1).

task_2(K) :-
    get_full_level(FullLevel),
    include(safe_2, FullLevel, SafeLevels),
    length(SafeLevels, K).
