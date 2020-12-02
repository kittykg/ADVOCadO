% nvim: set syntax=prolog

% AoC 2020 Day 2

:- consult('helper.pl').

count_char(_, [], 0).
count_char(X, [X|L], N) :-
    !, count_char(X, L, N1), N is N1 + 1.
count_char(X, [_|L], N) :-
    count_char(X, L, N).

valid(Pwd, Target, L, U) :-
    string_chars(Pwd, Chars), count_char(Target, Chars, Count),
    Count =< U, L =< Count.

valid_password(Str) :-
    split_string(Str, " ", "", [Bound, Tar, Pwd]),
    split_string(Bound, "-", "", [V1, V2]),
    string_to_number(V1, Lower), string_to_number(V2, Upper),
    string_chars(Tar, [Target, _]),
    valid(Pwd, Target, Lower, Upper).

char_at_index(Idx, List, Char, true) :- nth1(Idx, List, Char).
char_at_index(_, _, _, false).

xor(A,B) :- (A;B), not((A,B)).

valid_2(Pwd, Target, Idx1, Idx2) :-
    string_chars(Pwd, Chars),
    char_at_index(Idx1, Chars, Target, A),!,
    char_at_index(Idx2, Chars, Target, B),!,
    xor(A,B).

valid_password_2(Str) :-
    split_string(Str, " ", "", [Bound, Tar, Pwd]),
    split_string(Bound, "-", "", [V1, V2]),
    string_to_number(V1, Idx1), string_to_number(V2, Idx2),
    string_chars(Tar, [Target, _]),
    valid_2(Pwd, Target, Idx1, Idx2).

count_valid_passwords([], 0).
count_valid_passwords([S|L], Count) :-
    count_valid_passwords(L, C2),
    ( valid_password(S) ->
        Count is C2 + 1;
        Count is C2
    ).

task_1(Count) :-
    open('input_02', read, Stream),
    read_string_file(Stream, S),!,
    close(Stream),
    count_valid_passwords(S, Count).

count_valid_passwords_2([], 0).
count_valid_passwords_2([S|L], Count) :-
    count_valid_passwords_2(L, C2),
    ( valid_password_2(S) ->
        Count is C2 + 1;
        Count is C2
    ).

task_2(Count) :-
    open('input_02', read, Stream),
    read_string_file(Stream, S),!,
    close(Stream),
    count_valid_passwords_2(S, Count).
