% nvim: set syntax=prolog

% AoC 2022 Day 10

:- consult('../helper').

instruction((addx, N)) :-
    number(N).
instruction((noop, none)).

parse_line("noop", (noop, none)).
parse_line(S, (addx, N)) :-
    split_string(S, " ", "", ["addx", NS]),
    number_string(N, NS).

get_instructions(I) :-
    open('input_10', read, Stream),
    read_string_file(Stream, LS),
    maplist(parse_line, LS, I).

cycle([], _, []).
cycle([(noop, none)|I], CurrX, [CurrX|Res]) :-
    cycle(I, CurrX, Res).
cycle([(addx, N)|I], CurrX, [CurrX, NewX|Res]) :-
    NewX #= CurrX + N,
    cycle(I, NewX, Res).

get_signal_strength(XVals, C, SS) :-
    I #= C - 2,
    nth0(I, XVals, X),
    SS #= X * C.

task_1(Sum) :-
    get_instructions(I),
    cycle(I, 1, XVals),
    maplist(get_signal_strength(XVals), [20, 60, 100, 140, 180, 220], SS),
    sum_list(SS, Sum).

cycle_2_pixel(CurrX, CurrCrt, "#") :-
    CurrCrt #=< CurrX + 1,
    CurrCrt #>= CurrX - 1.
cycle_2_pixel(_, _, ".").

cycle_2_crt_pos(CurrCrt, 0) :-
    CurrCrt #= 40.
cycle_2_crt_pos(CurrCrt, CurrCrt).

cycle_2([], _, _, []).
cycle_2([(noop, none)|I], CurrX, CurrCrt, [Pixel|Res]) :-
    cycle_2_pixel(CurrX, CurrCrt, Pixel),
    IncCrt #= CurrCrt + 1,
    cycle_2_crt_pos(IncCrt, NewCrt),
    cycle_2(I, CurrX, NewCrt, Res).
cycle_2([(addx, N)|I], CurrX, CurrCrt, [Pixel1, Pixel2|Res]) :-
    cycle_2_pixel(CurrX, CurrCrt, Pixel1),
    Crt1 #= CurrCrt + 1,
    cycle_2_crt_pos(Crt1, Crt2),
    cycle_2_pixel(CurrX, Crt2, Pixel2),
    Crt3 #= Crt2 + 1,
    NewX #= CurrX + N,
    cycle_2_crt_pos(Crt3, NewCrt),
    cycle_2(I, NewX, NewCrt, Res).

print_pixels(6, _).
print_pixels(Row, Pixels) :-
    append(P1, P2, Pixels),
    length(P1, 40),
    maplist(write, P1),
    nl,
    NewRow #= Row + 1,
    print_pixels(NewRow, P2).

task_2() :-
    get_instructions(I),
    cycle_2(I, 1, 0, Pixels),
    print_pixels(0, Pixels).
