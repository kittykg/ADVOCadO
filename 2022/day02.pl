% nvim: set syntax=prolog

% AoC 2022 Day 2

:- consult('../helper.pl').

% RPS logic
win(r, p).
win(p, s).
win(s, r).
loss(r, s).
loss(p, r).
loss(s, p).

% A for Rock, B for Paper, and C for Scissors
opponent("A", r).
opponent("B", p).
opponent("C", s).

% 1 for Rock, 2 for Paper, and 3 for Scissors
point(r, 1).
point(p, 2).
point(s, 3).

% X for Rock, Y for Paper, and Z for Scissors
turn_1_guide("X", r).
turn_1_guide("Y", p).
turn_1_guide("Z", s).

% turn_1_point([+OpponentString, +String2], -Point)
turn_1_point([OS, MS], N) :-
    opponent(OS, O),
    turn_1_guide(MS, M),
    point(M, MP),
    win(O, M),
    N #= MP + 6.
turn_1_point([OS, MS], N) :-
    opponent(OS, O),
    turn_1_guide(MS, O),
    point(O, MP),
    N #= MP + 3.
turn_1_point([OS, MS], MP) :-
    opponent(OS, O),
    turn_1_guide(MS, M),
    point(M, MP),
    loss(O, M).

% turn_2_point([+OpponentString, +String2], -Point)
% String2: X to lose, Y to draw, Z to win
turn_2_point([OS, "X"], MP) :-
    opponent(OS, O),
    loss(O, M),
    point(M, MP).
turn_2_point([OS, "Y"], N) :-
    opponent(OS, O),
    point(O, MP),
    N #= MP + 3.
turn_2_point([OS, "Z"], N) :-
    opponent(OS, O),
    win(O, M),
    point(M, MP),
    N #= 6 + MP.

% parse_guide_line(+String, -Substrings)
parse_guide_line(S, SubStrings) :-
    split_string(S, " ", "", SubStrings).

% get_guide(-ParsedLines)
get_guide(PL) :-
    open('input_02', read, Stream),
    read_string_file(Stream, L),
    maplist(parse_guide_line, L, PL).

task_1(Sum) :-
    get_guide(PL),
    maplist(turn_1_point, PL, AllP),
    sum_list(AllP, Sum).

task_2(Sum) :-
    get_guide(PL),
    maplist(turn_2_point, PL, AllP),
    sum_list(AllP, Sum).
