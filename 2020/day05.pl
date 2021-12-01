% nvim :set syntax=prolog

% AoC 2020 Day 5

:- consult('../helper.pl').

bt(L, L, _, L).
bt(L, U, ['F'|C], Res) :-
    Pivot is (L + U) // 2,
    bt(L, Pivot, C, Res).
bt(L, U, ['L'|C], Res) :-
    Pivot is (L + U) // 2,
    bt(L, Pivot, C, Res).
bt(L, U, ['B'|C], Res) :-
    Pivot is (L + U + 1) // 2,
    bt(Pivot, U, C, Res).
bt(L, U, ['R'|C], Res) :-
    Pivot is (L + U + 1) // 2,
    bt(Pivot, U, C, Res).

seat_id(Str, Id) :-
    sub_string(Str, 0, 7, 3, Row),
    sub_string(Str, 7, 3, 0, Col),
    string_chars(Row, RowChars),
    string_chars(Col, ColChars),
    bt(0, 127, RowChars, RowId),
    bt(0, 7, ColChars, ColId),
    Id is RowId * 8 + ColId.

task_1(N) :-
    open('input_05', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream),
    maplist(seat_id, L, Res),
    max_list(Res, N).

task_2(N) :-
    open('input_05', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream),
    maplist(seat_id, L, Res),
    sort(Res, Sorted),
    min_list(Sorted, Min),
    max_list(Sorted, Max),
    numlist(Min, Max, L1),
    subtract(L1, Sorted, N).
