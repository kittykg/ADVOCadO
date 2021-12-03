% nvim: set syntax=prolog

% AoC 2021 Day 2

:- consult('../helper.pl').

% string_to_instr(+String, (-Direction, -Movement))
string_to_instr(S, (D, M)) :-
    split_string(S, " ", "", [D, MS]),
    atom_number(MS, M).

% update_coord((+Direction, +Movement),
%              (+CurrentH, +CurrentD),
%              (-NewH, -NewD))
update_coord(("forward", M), (HX, DX), (NewHX, DX)) :-
    NewHX is HX + M.
update_coord(("down", M), (HX, DX), (HX, NewDX)) :-
    NewDX is DX + M.
update_coord(("up", M), (HX, DX), (HX, NewDX)) :-
    NewDX is DX - M.

task_1(OUT) :-
    open('input_02', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(string_to_instr, SL, InstrL),
    foldl(update_coord, InstrL, (0,0), (OutH, OutD)),
    OUT is OutH * OutD.

% update_coord((+Direction, +Movement),
%              (+CurrentH, +CurrentD, +CurrntA),
%              (-NewH, -NewD, -NewA))
update_coord_2(("forward", M), (HX, DX, A), (NewHX, NewDX, A)) :-
    NewHX is HX + M,
    NewDX is DX + A * M.
update_coord_2(("down", M), (HX, DX, A), (HX, DX, NewA)) :-
    NewA is A + M.
update_coord_2(("up", M), (HX, DX, A), (HX, DX, NewA)) :-
    NewA is A - M.

task_2(OUT) :-
    open('input_02', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(string_to_instr, SL, InstrL),
    foldl(update_coord_2, InstrL, (0,0,0), (OutH, OutD,_)),
    OUT is OutH * OutD.
