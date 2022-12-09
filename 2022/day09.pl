% nvim: set syntax=prolog

% AoC 2022 Day 9

:- consult('../helper.pl').

% move_head_one(+Dir, +CurrPos, -NewPos)
move_head_one("R", (X, Y), (XNew, Y)) :-
    XNew #= X + 1.
move_head_one("L", (X, Y), (XNew, Y)) :-
    XNew #= X - 1.
move_head_one("U", (X, Y), (X, YNew)) :-
    YNew #= Y + 1.
move_head_one("D", (X, Y), (X, YNew)) :-
    YNew #= Y - 1.

% move_tail(+HeadPos, +CurrTailPos, -NewTailPos)
move_tail((HX, HY), (TX, TY), (TX, TY)) :-
    XDiff #= HX - TX,
    YDiff #= HY - TY,
    abs(XDiff, AXDiff),
    abs(YDiff, AYDiff),
    AXDiff #=< 1,
    AYDiff #=< 1.
move_tail((HX, HY), (TX, TY), (NewTX, NewTY)) :-
    XDiff #= HX - TX,
    YDiff #= HY - TY,
    move_tail_(XDiff, XMove),
    move_tail_(YDiff, YMove),
    NewTX #= TX + XMove,
    NewTY #= TY + YMove.

move_tail_(0, 0).
move_tail_(Diff, -1) :-
    Diff #< 0.
move_tail_(Diff, 1) :-
    Diff #> 0.

% move_1(+Movements, (+HeadPos, +TailPos), -Visited)
move_1([], _, []).
move_1([(Dir, Step)|Rest], (Head, Tail), FinalL) :-
    move_1_(Step, Dir, (Head, Tail), (NewHead, NewTail), L),
    move_1(Rest, (NewHead, NewTail), Out),
    append(L, Out, FinalL).

move_1_(0, _, P, P, []).
move_1_(N, Dir, (Head, Tail), (FinalHead, FinalTail), [NewTail|L]) :-
    move_head_one(Dir, Head, NewHead),
    move_tail(NewHead, Tail, NewTail),
    Next #= N - 1,
    move_1_(Next, Dir, (NewHead, NewTail), (FinalHead, FinalTail), L).

% move_2(+Movements, +KnotsList, -Visited)
move_2([], _, []).
move_2([(Dir, Step)|Rest], KnotsList, FinalL) :-
    move_2_(Step, Dir, KnotsList, NewKnotsList, L),
    move_2(Rest, NewKnotsList, Out),
    append(L, Out, FinalL).

move_2_(0, _, KnotsList, KnotsList, []).
move_2_(N, Dir, [Head|RestKnots], FinalKnotsList, [CurrTail|L]) :-
    move_head_one(Dir, Head, NewHead),
    move_other_knots([NewHead|RestKnots], NewKnotsList),
    last(NewKnotsList, CurrTail),
    Next #= N - 1,
    move_2_(Next, Dir, NewKnotsList, FinalKnotsList, L).

% move_other_knots(+Knots, -NewKnots)
move_other_knots([K1, K2], [K1, NewK2]) :-
    move_tail(K1, K2, NewK2).
move_other_knots([K1, K2|R], [K1|NewR]) :-
    move_tail(K1, K2, NewK2),
    move_other_knots([NewK2|R], NewR).

% parse_line(+Line, (-Direction, -Step))
parse_line(L, (Dir, Step)) :-
    split_string(L, " ", "", [Dir, StepStr]),
    number_string(Step, StepStr).

get_input(L) :-
    open('input_09', read, Stream),
    read_string_file(Stream, Lines),
    maplist(parse_line, Lines, L).

task_1(O) :-
    get_input(L),
    move_1(L, ((0, 0), (0, 0)), FL),
    list_to_set(FL, Set),
    length(Set, O).

task_2(O) :-
    get_input(L),
    repeat((0, 0), 10, InitialKnots),
    move_2(L, InitialKnots, FL),
    list_to_set(FL, Set),
    length(Set, O).
