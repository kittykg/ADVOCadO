% nvim: set syntax=prolog

% AoC 2022 Day 13

:- use_module(library(clpfd)).

% day13_input.pl should contain 2 predicates which return parsed lists:
% get_input(-List) and get_example_input(-L).
:- consult('./day13_input.pl').

% Integers
package_order(>, X, Y) :-
    number(X),
    number(Y),
    X #< Y.
package_order(=, X, X) :-
    number(X).
package_order(<, X, Y) :-
    number(X),
    number(Y),
    X #> Y.

% List
package_order(>, [], Y) :-
    is_list(Y),
    length(Y, YLen),
    YLen #> 0.
package_order(<, X, []) :-
    is_list(X),
    length(X, XLen),
    XLen #> 0.
package_order(=, [], []).
package_order(>, [X|_], [Y|_]) :-
    package_order(>, X, Y).
package_order(<, [X|_], [Y|_]) :-
    package_order(<, X, Y).
package_order(Order, [X|L1], [Y|L2]) :-
    package_order(=, X, Y),
    package_order(Order, L1, L2).

% Mix type
package_order(Order, X, Y) :-
    number(X),
    is_list(Y),
    package_order(Order, [X], Y).
package_order(Order, X, Y) :-
    is_list(X),
    number(Y),
    package_order(Order, X, [Y]).

% Task 1
task_1(Sum) :-
    get_input(L),
    task_1_(L, 1, Indices),
    sum_list(Indices, Sum).

task_1_([X, Y], Index, [Index]) :-
    package_order(>, X, Y).
task_1_([X, Y], _, []) :-
    package_order(<, X, Y).
task_1_([X, Y|L], Index, [Index|Res]) :-
    package_order(>, X, Y),
    NewIndex #= Index + 1,
    task_1_(L, NewIndex, Res).
task_1_([X, Y|L], Index, Res) :-
    package_order(<, X, Y),
    NewIndex #= Index + 1,
    task_1_(L, NewIndex, Res).

% Task 2
task_2(Prod) :-
    get_input(L),
    predsort(package_order, [[[2]], [[6]]|L], Sorted),
    % Sorted list is in descending order, need to calculate the actual index
    nth0(Index1, Sorted, [[2]]),
    nth0(Index2, Sorted, [[6]]),
    length(Sorted, Len),
    Prod #= (Len - Index1) * (Len - Index2).
