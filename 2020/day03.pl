% nvim :set syntax=prolog

% AoC 2020 Day 3

:- consult('helper.pl').

check_line(Line, LineNum, Modulo) :-
    string_chars(Line, LineChars),
    length(LineChars, Len),
    Idx is (LineNum * Modulo mod Len),
    nth0(Idx, LineChars, #).

count_trees([], _, _, 0).
count_trees([Line|L], LineNum, Modulo, Count) :-
    NextLineNum is LineNum + 1,
    count_trees(L, NextLineNum, Modulo, ResCount),
    (check_line(Line, LineNum, Modulo) ->
        Count is ResCount + 1;
        Count is ResCount
    ).

count_trees_2([], _, _, 0).
count_trees_2([_|L], LineNum, Modulo, Count) :-
    1 is LineNum mod 2, !,
    NextLineNum is LineNum + 1,
    count_trees_2(L, NextLineNum, Modulo, Count).
count_trees_2([Line|L], LineNum, Modulo, Count) :-
    NextLineNum is LineNum + 1,
    count_trees_2(L, NextLineNum, Modulo, ResCount),
    CurrLineNum is LineNum / 2,
    (check_line(Line, CurrLineNum, Modulo) ->
        Count is ResCount + 1;
        Count is ResCount
    ).

task_1(N) :-
    open('input_03', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream),
    count_trees(L, 0, 3, N).

task_2(N) :-
    open('input_03', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream),
    count_trees(L, 0, 1, N0),
    count_trees(L, 0, 3, N1),
    count_trees(L, 0, 5, N2),
    count_trees(L, 0, 7, N3),
    count_trees_2(L, 0, 1, N4),
    N is (N0 * N1 * N2 * N3 * N4).
