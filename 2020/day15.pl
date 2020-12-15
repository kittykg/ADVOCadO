% nvim :set syntax=prolog

% AoC 2020 Day 15

:- consult('helper.pl').

speak_num(Idx, LastNum, T, Out) :-
    V is Idx - 1,
    ( trie_lookup(T, LastNum, K) ->
        Out is V - K;
        Out is 0
    ),
    trie_update(T, LastNum, V).

get_nth_num(CurrIdx, CurrIdx, Last, _, Last).
get_nth_num(CurrIdx, Goal, LastNum, T, Res) :-
    speak_num(CurrIdx, LastNum, T, Out), !,
    NextIdx is CurrIdx + 1,
    get_nth_num(NextIdx, Goal, Out, T, Res).

get_input(L) :-
    open('input_15', read, Stream),
    read_line_to_string(Stream, S), !,
    close(Stream),
    split_string(S, ",", "", Sub),
    maplist(number_string, L, Sub).

construct_table([], [], _).
construct_table([X|L], [I|S], T) :-
    trie_insert(T, X, I), construct_table(L, S, T).

task_(K, N) :-
    get_input(L),
    length(L, Len), Upper is Len + 1,
    numlist(1, Len, Idices),
    trie_new(T), construct_table(L, Idices, T),
    tail(L, LastNum),
    get_nth_num(Upper, K, LastNum, T, N).

task_1(N) :- task_(2021, N).
task_2(N) :- task_(30000001, N).
