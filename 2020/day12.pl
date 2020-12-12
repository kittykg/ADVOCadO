% nvim :set syntax=prolog

% AoC 2020 Day 11

:- consult('helper.pl').

parse_line(S, (Dir, Num)) :-
    sub_string(S, 0, 1, _, Dir),
    sub_string(S, 1, _, 0, NumStr), string_to_number(NumStr, Num).

dir_to_movement("E", N, N, 0).
dir_to_movement("N", N, 0, N).
dir_to_movement("W", N, Dx, 0) :- Dx is -N.
dir_to_movement("S", N, 0, Dy) :- Dy is -N.

rotate_R("N", "E").
rotate_R("E", "S").
rotate_R("S", "W").
rotate_R("W", "N").

rotate_L("N", "W").
rotate_L("W", "S").
rotate_L("S", "E").
rotate_L("E", "N").

move(("F", Num), (X, Y, Dir), (NewX, NewY, Dir)) :-
    dir_to_movement(Dir, Num, Dx, Dy),
    NewX is X + Dx, NewY is Y + Dy.
move(("L", 90),  (X, Y, Dir), (X, Y, NewDir)) :- rotate_L(Dir, NewDir).
move(("L", 180), (X, Y, Dir), (X, Y, NewDir)) :- rotate_L(Dir, D), rotate_L(D, NewDir).
move(("L", 270), (X, Y, Dir), (X, Y, NewDir)) :- rotate_R(Dir, NewDir).
move(("R", 90),  (X, Y, Dir), (X, Y, NewDir)) :- rotate_R(Dir, NewDir).
move(("R", 180), (X, Y, Dir), (X, Y, NewDir)) :- rotate_R(Dir, D), rotate_R(D, NewDir).
move(("R", 270), (X, Y, Dir), (X, Y, NewDir)) :- rotate_L(Dir, NewDir).
move((D, Num), (X, Y, Dir), (NewX, NewY, Dir)) :-
    dir_to_movement(D, Num, Dx, Dy),
    NewX is X + Dx, NewY is Y + Dy.

rotate_L_coord((X, Y), (NewY, X)) :- NewY is -Y.
rotate_R_coord((X, Y), (Y, NewX)) :- NewX is -X.
rotate_180_coord((X, Y), (NewX, NewY)) :- NewX is -X, NewY is -Y.

move_2(("F", Num), (SX, SY), (WX, WY), (NewSX, NewSY), (WX, WY)) :-
    DX is WX * Num, DY is WY * Num,
    NewSX is SX + DX, NewSY is SY + DY.
move_2(("L", 90), (SX, SY), (WX, WY), (SX, SY), (NewWX, NewWY)) :-
    rotate_L_coord((WX, WY), (NewWX, NewWY)).
move_2(("L", 270), (SX, SY), (WX, WY), (SX, SY), (NewWX, NewWY)) :-
    rotate_R_coord((WX, WY), (NewWX, NewWY)).
move_2(("R", 90), (SX, SY), (WX, WY), (SX, SY), (NewWX, NewWY)) :-
    rotate_R_coord((WX, WY), (NewWX, NewWY)).
move_2(("R", 270), (SX, SY), (WX, WY), (SX, SY), (NewWX, NewWY)) :-
    rotate_L_coord((WX, WY), (NewWX, NewWY)).
move_2((Cmd, 180), (SX, SY), (WX, WY), (SX, SY), (NewWX, NewWY)) :-
    (Cmd == "L"; Cmd =="R"), !,
    rotate_180_coord((WX, WY), (NewWX, NewWY)).
move_2((Cmd, Num), (SX, SY), (WX, WY), (SX, SY), (NewWX, NewWY)) :-
    move((Cmd, Num), (WX, WY, _), (NewWX, NewWY, _)).

get_input(L) :-
    open('input_12', read, Stream),
    read_string_file(Stream, L1), !,
    close(Stream),
    maplist(parse_line, L1, L).

task_1(N) :-
    get_input(L), foldl(move, L, (0, 0, "E"), (FinalX, FinalY, _)),
    abs(FinalX, AbsX), abs(FinalY, AbsY), N is AbsX + AbsY.

task_2_((Cmd, Num), CurrState, NextState) :-
    CurrState = ((SX, SY), (WX, WY)),
    move_2((Cmd, Num), (SX, SY), (WX, WY), (NSX, NSY), (NWX, NWY)),
    NextState = ((NSX, NSY), (NWX, NWY)).

task_2(N) :-
    get_input(L), foldl(task_2_, L, ((0,0), (10,1)), FinalState),
    FinalState = ((SX, SY), (_, _)),
    abs(SX, AbsX), abs(SY, AbsY), N is AbsX + AbsY.
