% nvim: set syntax=prolog

% AoC 2023 Day 1

:- use_module(library(clpfd)).
:- consult('../helper.pl').

is_digit('0', 0).
is_digit('1', 1).
is_digit('2', 2).
is_digit('3', 3).
is_digit('4', 4).
is_digit('5', 5).
is_digit('6', 6).
is_digit('7', 7).
is_digit('8', 8).
is_digit('9', 9).

is_digit_str("zero", 0).
is_digit_str("one", 1).
is_digit_str("two", 2).
is_digit_str("three", 3).
is_digit_str("four", 4).
is_digit_str("five", 5).
is_digit_str("six", 6).
is_digit_str("seven", 7).
is_digit_str("eight", 8).
is_digit_str("nine", 9).

combine_digits(D1, D2, N) :-
    N #= D1 * 10 + D2.

% get_input(-Lines)
get_input(L) :-
    open('input_01', read, Stream),
    read_string_file(Stream, L).

% get_first_digit_1(+Head, +Rest, -FirstDigit)
get_first_digit_1(Head, _, FirstDigit) :-
    is_digit(Head, FirstDigit).
get_first_digit_1(_, Rest, FirstDigit) :-
    string_chars(Rest, [NewHead|NewRest]),
    get_first_digit_1(NewHead, NewRest, FirstDigit).

% get_last_digit_1(+Rest, +Tail, -LastDigit)
get_last_digit_1(_, Tail, LastDigit) :-
    is_digit(Tail, LastDigit).
get_last_digit_1(Rest, _, LastDigit) :-
    string_chars(Rest, RestChar),
    append(NewRest, [NewTail], RestChar),
    get_last_digit_1(NewRest, NewTail, LastDigit).

% get_number_1(+Line, -Number)
get_number_1(L, N) :-
    get_first_digit_1('', L, FirstDigit),
    get_last_digit_1(L, '', LastDigit),
    combine_digits(FirstDigit, LastDigit, N).

task_1(K) :-
    get_input(L),
    maplist(get_number_1, L, N),
    sum_list(N, K).

% get_first_digit_2(+HeadChars, +Rest, -FirstDigit)
get_first_digit_2(HeadChars, _, FirstDigit) :-
    last(HeadChars, Head),
    is_digit(Head, FirstDigit).
get_first_digit_2(HeadChars, _, FirstDigit) :-
    append(_, Sub, HeadChars),
    string_chars(SubStr, Sub),
    is_digit_str(SubStr, FirstDigit).
get_first_digit_2(HeadChars, Rest, FirstDigit) :-
    string_chars(Rest, [NewHead|NewRest]),
    append(HeadChars, [NewHead], NewHeadChars),
    get_first_digit_2(NewHeadChars, NewRest, FirstDigit).

% get_last_digit_2(+Rest, +TailChars, -LastDigit)
get_last_digit_2(_, [T|_], LastDigit) :-
    is_digit(T, LastDigit).
get_last_digit_2(_, TailChars, LastDigit) :-
    append(Sub, _, TailChars),
    string_chars(SubStr, Sub),
    is_digit_str(SubStr, LastDigit).
get_last_digit_2(Rest, TailChars, LastDigit) :-
    string_chars(Rest, RestChar),
    append(NewRest, [NewTail], RestChar),
    get_last_digit_2(NewRest, [NewTail|TailChars], LastDigit).

% get_number_2(+Line, -Number)
get_number_2(L, N) :-
    get_first_digit_2([], L, FirstDigit),
    get_last_digit_2(L, [], LastDigit),
    combine_digits(FirstDigit, LastDigit, N).

task_2(K) :-
    get_input(L),
    maplist(get_number_2, L, N),
    sum_list(N, K).
