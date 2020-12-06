% nvim :set syntax=prolog

% AoC 2020 Day 6

:- consult('helper.pl').

string_to_set(String, Set) :-
    string_chars(String, Chars),
    list_to_set(Chars, Set).

check_answer(S, Count) :-
    string_to_set(S, Set),
    length(Set, Count).

count_ans([], Buffer, Count) :-
    check_answer(Buffer, Count).
count_ans([""|L], Buffer, Count) :-
    count_ans(L, "", Res),
    check_answer(Buffer, Check),
    Count is Res + Check.
count_ans([X|L], Buffer, Count) :-
    string_concat(Buffer, X, S),
    count_ans(L, S, Count).

task_1(N) :-
    open('input_06', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream),
    count_ans(L, "", N).

% -----------------------------------

all_yes_ans(_, [], 0).
all_yes_ans([], Acc, C) :- length(Acc, C).
all_yes_ans([X|L], Acc, Count) :-
    string_to_set(X, Set),
    intersection(Set, Acc, Intersect),
    all_yes_ans(L, Intersect, Count).

count_all_yes_ans([], [X|L], Count) :-
    string_to_set(X, Set),
    all_yes_ans([X|L], Set, Count).
count_all_yes_ans([""|L], [X|M], Count) :-
    count_all_yes_ans(L, [], Res),
    string_to_set(X, Set),
    all_yes_ans([X|M], Set, Check),
    Count is Res + Check.
count_all_yes_ans([X|L], Buffer, Count) :-
    count_all_yes_ans(L, [X|Buffer], Count).

task_2(N) :-
    open('input_06', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream),
    count_all_yes_ans(L, [], N).
