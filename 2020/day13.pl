% nvim :set syntax=prolog

% AoC 2020 Day 13

:- consult('../helper.pl').

calc_wait(TS, Id, Wait) :- Wait is Id - (TS mod Id).

wait_to_id(Wait, TS, Ids, Id) :- member(Id, Ids), Id is Wait + (TS mod Id).

get_input(TS, Subs) :-
    open('input_13', read, Stream),
    read_number_line(Stream, TS),
    read_line_to_string(Stream, S), !,
    close(Stream),
    split_string(S, ",", "", Subs).

task_1(N) :-
    get_input(TS, Subs),
    include(\=("x"), Subs, IdStr),
    maplist(number_string, Ids, IdStr),
    maplist(calc_wait(TS), Ids, Waits),
    min_list(Waits, W), wait_to_id(W, TS, Ids, Id),
    N is W * Id.

parse([], _, [], []).
parse(["x"|Subs], Ri, B, M) :- NextRi is Ri + 1, parse(Subs, NextRi, B, M).
parse([NumStr|Subs], Ri, [Bi|B], [Mi|M]) :-
    number_string(Mi, NumStr),
    Bi is (Mi - Ri) mod Mi,
    NextRi is Ri + 1,
    parse(Subs, NextRi, B, M).

xi_(N, Xi, K, Xi) :- R is (N * Xi) mod K, R == 1.
xi_(N, Xi, K, Res) :- NXi is Xi + 1, xi_(N, NXi, K, Res).

c_r_Ni(Mi, Idx, Ni) :-
    split_at(Idx, Mi, S1, [_|S2]),
    prod_list(S1, P1), prod_list(S2, P2),
    Ni is P1 * P2.

c_r_Xi(Ni, Mi, Xi) :- N is Ni mod Mi, xi_(N, 1, Mi, Xi).

c_r_BiNiXi(Bi, Ni, Xi, BiNiXi) :- BiNiXi is Bi * Ni * Xi.

chinese_remainder(B, M, Res) :-
    enum_list(M, Idices),
    maplist(c_r_Ni(M), Idices, N),
    maplist(c_r_Xi, N, M, X),
    maplist(c_r_BiNiXi, B, N, X, BNX),
    sum_list(BNX, SumBNX), prod_list(M, ProdM),
    Res is SumBNX mod ProdM.

task_2(N) :-
    get_input(_, Subs), parse(Subs, 0, B, M),
    chinese_remainder(B, M, N).
