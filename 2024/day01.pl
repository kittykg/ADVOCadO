% nvim: set syntax=prolog

% AoC 2024 Day 1

:- use_module(library(clpfd)).
:- consult('../helper.pl').

% get_input(-Lines)
get_input(L) :-
    open('input_01', read, Stream),
    read_string_file(Stream, L).

% get_numbers_from_line(+Line, -N1, -N2)
get_numbers_from_line(Line, N1, N2) :-
    split_string(Line, " ", "", SL),
    head(SL, N1String), last(SL, N2String),
    atom_number(N1String, N1), atom_number(N2String, N2).

% get_number_list(+Lines, -L1, -L2)
get_number_list([], [], []).
get_number_list([H|T], [N1|N1T], [N2|N2T]) :-
    get_numbers_from_line(H, N1, N2),
    get_number_list(T, N1T, N2T).


task_1_aux(X1, X2, Acc, Res) :- Res #= Acc + abs(X1 - X2).

task_1(K) :-
    get_input(L),
    get_number_list(L, N1, N2),
    msort(N1, N1Sorted), msort(N2, N2Sorted),
    foldl(task_1_aux, N1Sorted, N2Sorted, 0, K).

% get_similarity_score(+N, +Counter1, +Counter2, -Res)
get_similarity_score(N, Counter1, Counter2, Res) :-
    trie_lookup(Counter1, N, Occ1),
    (trie_gen(Counter2, N), ! ->
        trie_lookup(Counter2, N, Occ2),
        Res #= Occ1 * N * Occ2;
        Res #= 0
    ).

task_2(K) :-
    get_input(L),
    get_number_list(L, N1, N2),
    get_counter_trie(N1, T1), get_counter_trie(N2, T2),
    get_trie_keys(T1, UniqueNumbers),
    maplist(
        {T1, T2}/[Number, Score]>>get_similarity_score(Number, T1, T2, Score),
        UniqueNumbers,
        Scores
    ),
    sumlist(Scores, K).
