% nvim :set syntax=prolog

% AoC 2020 Day 14

:- consult('helper.pl').

mask_re(S, Mask) :-
    re_matchsub("^mask = (?<mask>[10X]{36})$", S, Sub, []),
    MaskStr = Sub.get(mask), string_chars(MaskStr, Mask).

mem_re(S, Addr, Val) :-
    re_matchsub("^mem\\[(?<addr_N>[0-9]+)\\] = (?<val_N>[0-9]+)$", S, Sub,
                [capture_type]),
    Addr = Sub.get(addr), Val = Sub.get(val).

dec_to_bin(0, ['0']).
dec_to_bin(1, ['1']).
dec_to_bin(Val, Bs) :-
    Val > 1, X is Val mod 2, number_chars(X, C),
    Y is Val // 2, dec_to_bin(Y, B1),
    append(B1, C, Bs).

bin_to_dec(B, Dec) :- bin_to_dec_(B, 0, Dec).
bin_to_dec_([], Acc, Acc).
bin_to_dec_([X|L], Acc, Dec) :-
    number_chars(N, [X]),
    Acc1 is Acc * 2 + N,
    bin_to_dec_(L, Acc1, Dec).

fill_to_36_bits(B, B_36) :-
    length(B, Len), Diff is 36 - Len,
    repeat('0', Diff, Zeros), append(Zeros, B, B_36).

to_bits(V, B_36) :- dec_to_bin(V, Bs), fill_to_36_bits(Bs, B_36).

write_to_mem(Val, Addr, Mem, NewMem) :- NewMem = Mem.put([Addr = Val]).

get_input(L) :-
    open('input_14', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream).

% --------------------- Task 1 ---------------------

apply_mask_(V, 'X', V).
apply_mask_(_, M, M).
apply_mask(Mask, B_36, Res) :- maplist(apply_mask_, B_36, Mask, Res).

task_1_(S, (_, CurrMem), (NewMask, CurrMem)) :- mask_re(S, NewMask).
task_1_(S, (CurrMask, CurrMem), (CurrMask, NewMem)) :-
    mem_re(S, Addr, Val), to_bits(Val, B_36),
    apply_mask(CurrMask, B_36, Res),
    bin_to_dec(Res, DecRes),
    write_to_mem(DecRes, Addr, CurrMem, NewMem).

task_1(N) :-
    get_input(L),
    foldl(task_1_, L, ([], mem{}), (_, FinalMem)),
    get_all_val(FinalMem, Vals), sum_list(Vals, N).

% --------------------- Task 2 ---------------------

apply_mask_2_(V, '0', V).
apply_mask_2_(_, M, M).
apply_mask_2(Mask, B_36, Res) :- maplist(apply_mask_2_, B_36, Mask, Res).

mutate(B_36, Res) :- mutate_(B_36, [], Comb), maplist(reverse, Comb, Res).
mutate_([], Acc, [Acc]).
mutate_(['X'|L], Acc, Set) :-
    mutate_(L, ['0'|Acc], Set1), !,
    mutate_(L, ['1'|Acc], Set2), !,
    append(Set1, Set2, Set).
mutate_([B|L], Acc, Set) :- mutate_(L, [B|Acc], Set).

task_2_(S, (_, CurrMem), (NewMask, CurrMem)) :- mask_re(S, NewMask).
task_2_(S, (CurrMask, CurrMem), (CurrMask, NewMem)) :-
    mem_re(S, Addr, Val), to_bits(Addr, B),
    apply_mask_2(CurrMask, B, B_36),
    mutate(B_36, B_Addrs), maplist(bin_to_dec, B_Addrs, Addrs),
    foldl(write_to_mem(Val), Addrs, CurrMem, NewMem).

task_2(N) :-
    get_input(L),
    foldl(task_2_, L, ([], mem{}), (_, FinalMem)),
    get_all_val(FinalMem, Vals), sum_list(Vals, N).
