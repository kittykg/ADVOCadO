% nvim :set syntax=prolog

% AoC 2020 Day 4

:- use_module(library(pcre)).
:- consult('helper.pl').

get_field(S, F) :- sub_string(S, 0, 3, _, F).

extract_fields([], []).
extract_fields([X|L], Fields) :-
    extract_fields(L, Res),
    split_string(X, " ", "", Strs),
    maplist(get_field, Strs, Fs),
    append(Fs, Res, Fields).

validate_fields(Fields) :- length(Fields, 8).
validate_fields(Fields) :-
    length(Fields, 7), \+ member("cid", Fields).

validate([], Buffer, Count) :-
    extract_fields(Buffer, Fields),
    (validate_fields(Fields) ->
        Count is 1;
        Count is 0
    ).
validate([""|L], Buffer, Count) :-
    validate(L, [], Res),
    extract_fields(Buffer, Fields),
    (validate_fields(Fields) ->
        Count is 1 + Res;
        Count is Res
    ).
validate([X|L], Buffer, Count) :- validate(L, [X|Buffer], Count).

task_1(N) :-
    open('input_04', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream),
    validate(L, [], N).

% ----------------------
% ----------------------

valid_year(S, L, U) :-
    sub_string(S, 4, 4, 0, Val),
    string_to_number(Val, Y),
    Y >= L, Y =< U.

valid_byr(S) :-
    sub_string(S, 0, 3, _, "byr"),
    valid_year(S, 1920, 2002).

valid_iyr(S) :-
    sub_string(S, 0, 3, _, "iyr"),
    valid_year(S, 2010, 2020).

valid_eyr(S) :-
    sub_string(S, 0, 3, _, "eyr"),
    valid_year(S, 2020, 2030).

valid_hgt(S) :-
    re_match("hgt:[0-9]{3}cm$"/i, S),
    sub_string(S, 4, 3, 2, Val),
    string_to_number(Val, H),
    H >= 150, H =< 193.
valid_hgt(S) :-
    re_match("hgt:[0-9]{2}in$"/i, S),
    sub_string(S, 4, 2, 2, Val),
    string_to_number(Val, H),
    H >= 59, H =< 76.

valid_hcl(S) :- re_match("^hcl:#[0-9a-f]{6}$"/i, S).

valid_ecl(S) :-
    sub_string(S, 0, 3, _, "ecl"),
    sub_string(S, 4, 3, 0, Clr),
    member(Clr, ["amb", "blu", "brn", "gry", "grn", "hzl","oth"]).

valid_pid(S) :- re_match("^pid:[0-9]{9}$"/i, S).

validate_field_2(S) :-
    valid_byr(S), !;
    valid_iyr(S), !;
    valid_eyr(S), !;
    valid_hgt(S), !;
    valid_hcl(S), !;
    valid_ecl(S), !;
    valid_pid(S).

extract_valid_fields([], []).
extract_valid_fields([X|L], Fields) :-
    extract_valid_fields(L, Res),
    split_string(X, " ", "", Strs),
    include(validate_field_2, Strs, Fs),
    append(Fs, Res, Fields).

validate_2([], Buffer, Count) :-
    extract_valid_fields(Buffer, Fields),
    (validate_fields(Fields) ->
        Count is 1;
        Count is 0
    ).
validate_2([""|L], Buffer, Count) :-
    validate_2(L, [], Res),
    extract_valid_fields(Buffer, Fields),
    (validate_fields(Fields) ->
        Count is 1 + Res;
        Count is Res
    ).
validate_2([X|L], Buffer, Count) :- validate_2(L, [X|Buffer], Count).

task_2(N) :-
    open('input_04', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream),
    validate_2(L, [], N).
