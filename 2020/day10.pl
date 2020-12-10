% nvim :set syntax=prolog

% AoC 2020 Day 10

:- consult('helper.pl').

is_one(1).
is_three(3).

task_1_(L, N) :-
    sort(L, L1), length(L, Len),
    sub_list([0|L1], 0, Len, L2),
    maplist(minus, L1, L2, Diff),
    include(is_one, Diff, Ones), length(Ones, NumOne),
    include(is_three, Diff, Threes), length(Threes, NumThree),
    N is NumOne * (NumThree + 1).

task_1(N) :-
    open('input_10', read, Stream),
    read_number_file(Stream, L), !,
    close(Stream),
    task_1_(L, N).

get_ways(0, _, 1).
get_ways(N, HT, 0) :- \+ _ = HT.get(N).
get_ways(N, HT, A) :- A = HT.get(N).

ways_to_0(N, HT, NewHT) :-
    V1 is N - 1, get_ways(V1, HT, W1),
    V2 is N - 2, get_ways(V2, HT, W2),
    V3 is N - 3, get_ways(V3, HT, W3),
    W is W1 + W2 + W3,
    NewHT = HT.put([N = W]).

task_2_(L, N) :-
    sort(L, SortedL), max_list(SortedL, Max),
    foldl(ways_to_0, SortedL, ht{}, FinalHT),
    N = FinalHT.get(Max).

task_2(N) :-
    open('input_10', read, Stream),
    read_number_file(Stream, L), !,
    close(Stream),
    task_2_(L, N).
