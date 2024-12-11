% nvim: set syntax=prolog

% AoC 2024 Day 11

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- consult('../helper.pl').

:- initialization(main, main).

% ---------- DCG ----------

input(Input) --> sequence(integer, (blank, \+ eos), Input), blank.

get_input(Input) :- phrase_from_file(input(Input), 'input_11').

% ---------- Part 1 & 2 ----------

stone_split(0, [1]) :- !.
stone_split(S, [S1, S2]) :-
    number_codes(S, C),
    append(C1, C2, C),
    length(C1, Len), length(C2, Len), !,
    number_codes(S1, C1), number_codes(S2, C2).
stone_split(S, [S1]) :- S1 #= S * 2024, !.

blink(StoneCountTrie, NewStoneCountTrie) :-
    trie_new(NewStoneCountTrie),
    get_trie_items(StoneCountTrie, Items), !,
    maplist(blink_(NewStoneCountTrie), Items).

blink_(NewStoneCountTrie, (Stone, Count)) :-
    stone_split(Stone, NewStones),
    maplist(blink__(NewStoneCountTrie, Count), NewStones).

blink__(NewStoneCountTrie, Count, NewStone) :-
    (
        trie_gen(NewStoneCountTrie, NewStone), ! ->
            trie_lookup(NewStoneCountTrie, NewStone, V),
            NewV #= V + Count,
            trie_update(NewStoneCountTrie, NewStone, NewV);
            trie_insert(NewStoneCountTrie, NewStone, Count)
    ).

get_initial_trie(InitialTrie) :-
    get_input(Input),
    get_counter_trie(Input, InitialTrie).

blink_n_times(N, TotalStones) :-
    get_initial_trie(InitialTrie),
    blink_n_times_(N, InitialTrie, FinalTrie),
    get_trie_vals(FinalTrie, Counts),
    sum_list(Counts, TotalStones).

blink_n_times_(0, T, T) :- !.
blink_n_times_(N, StoneCountTrie, TotalStones) :-
    N > 0, NextN #= N - 1, !,
    blink(StoneCountTrie, NewStoneCountTrie),
    blink_n_times_(NextN, NewStoneCountTrie, TotalStones).

main(_) :-
    % task 1
    blink_n_times(25, K1),
    format("Task 1: ~d\n", K1),
    % task 2
    blink_n_times(75, K2),
    format("Task 2: ~d\n", K2).
