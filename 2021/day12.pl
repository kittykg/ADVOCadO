% nvim: set syntax=prolog

% AoC 2021 Day 12

:- consult('../helper.pl').
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).

%-----------------------------------%
%            IO parsing             %
%-----------------------------------%

split_delimiter(S, As) :-
    split_string(S, "-", "", L),
    maplist(string_to_atom_, L, As).

string_to_atom_(S, A) :-
    atom_string(A, S).

update_link_trie(Trie, [S, E]) :-
    trie_gen(Trie, S), !,
    trie_lookup(Trie, S, CurrList),
    trie_update(Trie, S, [E|CurrList]).
update_link_trie(Trie, [S, E]) :-
    trie_insert(Trie, S, [E]).

get_link_trie(Trie) :-
    open('input_12', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(split_delimiter, SL, Splits),
    trie_new(Trie),
    maplist(update_link_trie(Trie), Splits).

small_cave(P) :-
    P \= start, P \= end, char_type(P, lower).

visit(_, end, _, 1).
visit(_, CurrPoint, [_], 0) :- small_cave(CurrPoint), !.
visit(Trie, CurrPoint, SmallCave, Count) :-
    trie_lookup(Trie, CurrPoint, PD),
    ( small_cave(CurrPoint), ! ->
        UpdatedSmallCave = [CurrPoint|SmallCave];
        UpdatedSmallCave = SmallCave
    ),
    maplist(\X^(visit(Trie, X, UpdatedSmallCave)), PD, Counts),
    sum_list(Counts, Count).

task_1(Out) :- get_link_trie(Trie), visit(Trie, start, [], Out).