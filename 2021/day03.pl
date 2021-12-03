% nvim: set syntax=prolog

% AoC 2021 Day 3

:- consult('../helper.pl').
:- use_module(library(clpfd)).

% bin_to_dec(+BitList, -Number)
bin_to_dec(B, N) :-
    reverse(B, BR),
    bin_to_dec_(BR, 0, 0, N).

bin_to_dec_([], _, N, N).
bin_to_dec_([B|Bs], I0, N0, N) :-
        N1 #= N0 + (2^I0)*B,
        I1 #= I0 + 1,
        bin_to_dec_(Bs, I1, N1, N).

% get_common(+List, -MostCommon, -LeastCommon)
get_common(L, MC, LC) :-
    count(1, L, N1),
    count(0, L, N0), !,
    ( N1 > N0 -> MC = 1, LC = 0 ; MC = 0, LC = 1).

% acc_common(+List, -GammaList, -EpsilonList)
acc_common([], [], []).
acc_common([L|R], [MC|NewGamma], [LC|NewEpsilon]) :-
    get_common(L, MC, LC),
    acc_common(R, NewGamma, NewEpsilon).

% binary_convertion(+Code, -Bit)
binary_convertion(49, 1).
binary_convertion(48, 0).

% string_to_binary(+String, -BitList)
string_to_binary(S, BL) :-
    string(S),
    string_codes(S, SC),
    maplist(binary_convertion, SC, BL).

task_1(OUT) :-
    open('input_03', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(string_to_binary, SL, SC),
    transpose(SC, SCT),
    acc_common(SCT, GammaB, EpsilonB),
    bin_to_dec(GammaB, GammaD), bin_to_dec(EpsilonB, EpsilonD),
    OUT #= GammaD * EpsilonD.

% get_common_2(+List, +Type, -Goal)
get_common_2(L, o, MC) :-
    count(1, L, N1),
    count(0, L, N0), !,
    ( N1 >= N0 -> MC = 1 ; MC = 0).
get_common_2(L, c, LC) :-
    count(1, L, N1),
    count(0, L, N0), !,
    ( N1 >= N0 -> LC = 0 ; LC = 1).

% filter_number(+Index, +Goal, +BitList)
filter_number(Index, Goal, X) :-
    nth1(Index, X, Elem),
    Elem == Goal.

% update(+List, +Type, +Index, -NewList)
update(L, _, _, L) :- length(L, 1).
update(L, Type, Index, FinL) :-
    transpose(L, LT),
    nth1(Index, LT, Digits),
    get_common_2(Digits, Type, Goal),
    include(filter_number(Index, Goal), L, NewL),
    NewIndex #= Index + 1,
    update(NewL, Type, NewIndex, FinL).

task_2(OUT) :-
    open('input_03', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(string_to_binary, SL, BL),
    update(BL, o, 1, [OL]),
    update(BL, c, 1, [CL]),
    bin_to_dec(OL, O), bin_to_dec(CL, C),
    OUT #= O * C.
