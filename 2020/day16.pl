% nvim :set syntax=prolog

% AoC 2020 Day 16

:- consult('../helper.pl').
:- use_module(library(clpfd)).

% -------------- Parsing --------------

get_range(S, N1, N2, N3, N4) :-
    re_matchsub(
        "^[a-z ]+: (?<n1_N>[0-9]+)-(?<n2_N>[0-9]+) or (?<n3_N>[0-9]+)-(?<n4_N>[0-9]+)$",
        S, Sub, [capture_type]),
    N1 = Sub.get(n1), N2 = Sub.get(n2), N3 = Sub.get(n3), N4 = Sub.get(n4).

get_ticket(Str, T) :-
    split_string(Str, ",", "", Sub),
    maplist(number_string, T, Sub).

parse_input([X|L], [R1, R2|Fields], MyT, NT) :-
    get_range(X, N1, N2, N3, N4),
    R1 = (N1, N2), R2 = (N3, N4), !,
    parse_input(L, Fields, MyT, NT).
parse_input(["", X, Str|L], [], MyT, NT) :-
    X == "your ticket:", get_ticket(Str, MyT),
    parse_input(L, [], MyT, NT).
parse_input(["", X|L], [], _, NT):-
    X == "nearby tickets:",
    maplist(get_ticket, L, NT).

get_input(L) :-
    open('input_16', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream).

task_input(Field, MyT, NT) :-
    get_input(L), parse_input(L, Field, MyT, NT).

% -------------- Task 1 --------------

gen_range(Fields, Range) :-
    foldl(gen_range_, Fields, [], RangeList), list_to_set(RangeList, Range).
    
gen_range_((L, U), V0, V1) :- numlist(L, U, R), append(V0, R, V1).

invalid(Ranges, N) :- \+ member(N, Ranges).

sum_invalid_num(Ranges, List, V0, V1) :-
    include(invalid(Ranges), List, Invalid),
    sum_list(Invalid, Sum),
    V1 #= Sum + V0.

invalid_ticket(Ranges, List) :-
    include(invalid(Ranges), List, Invalid),
    length(Invalid, Len), Len #> 0.

task_1(N) :-
    task_input(Field, _, NT), !,
    gen_range(Field, Range),
    foldl(sum_invalid_num(Range), NT, 0, N).
