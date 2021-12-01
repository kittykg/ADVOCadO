% AoC 2021 Day 1

:- consult('../helper.pl').

% find_increase(+List, -Value)
find_increase([_], 0).
find_increase([X, Y|L], N2) :-
    X > Y, !, find_increase([Y|L], N1), N2 is N1 + 1.
find_increase([_, Y|L], N) :-
    find_increase([Y|L], N).

task_1(Out) :-
    open('input_01', read, Stream),
    read_number_file(Stream, Ns), !,
    close(Stream),
    reverse(Ns, RNs),
    find_increase(RNs, Out).

% three_measure_sliding_window(+List1, -List2)
three_measure_sliding_window([X, Y, Z], [N]) :- N is (X + Y + Z).
three_measure_sliding_window([X, Y, Z|L], [N|L2]) :-
    N is (X + Y + Z),
    three_measure_sliding_window([Y, Z|L], L2).

task_2(Out) :-
    open('input_01', read, Stream),
    read_number_file(Stream, Ns), !,
    close(Stream),
    three_measure_sliding_window(Ns, SWNs),
    reverse(SWNs, RNs),
    find_increase(RNs, Out).
