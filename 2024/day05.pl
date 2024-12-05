% nvim: set syntax=prolog

% AoC 2024 Day 5

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

% ---------- DCG ----------
input(Orders, Updates) --> orders(Orders), updates(Updates).

orders([]) --> "\n".
orders([A-B|Os]) --> order(A-B), orders(Os).

order(A-B) --> integer(A), "|", integer(B), "\n".

updates([]) --> (eos ; "\n").
updates([U|Us]) --> update(U), updates(Us).

update([A]) --> integer(A), "\n".
update([A|U]) --> integer(A), ",", update(U).

get_orders_and_updates(Orders, Updates) :-
    phrase_from_file(input(Orders, Updates), 'input_05').

% ---------- Solution ----------

% safe(+Orders, +Update)
safe(_, []).
safe(Orders, [U|Us]) :-
    maplist({Orders, U}/[N2]>>safe_(Orders, U, N2), Us),
    safe(Orders, Us).

safe_(Orders, N1, N2) :- \+ member(N2-N1, Orders).

% middle_number(+List, -Middle)
middle_number(L, M) :-
    append(L1, [M|L2], L),
    length(L1, N), length(L2, N).

task_1(K) :-
    get_orders_and_updates(Orders, Updates),
    convlist({Orders}/[U,M]>>(safe(Orders,U), middle_number(U,M)), Updates, Mids),
    sum(Mids, #=, K).

% change_unsafe(+Orders, +UnsafeUpdate, -NewSafeUpdate)
change_unsafe(Orders, U, U) :- safe(Orders, U), !.
change_unsafe(Orders, U, NewU) :-
    append(L1, [N1|L2], L3),
    append(L3, [N2|L4], U),
    \+ safe_(Orders, N1, N2),
    append(L1, [N2|L2], NewU1),
    append(NewU1, [N1|L4], NewU2),
    change_unsafe(Orders, NewU2, NewU).

task_2(K) :-
    get_orders_and_updates(Orders, Updates),
    exclude({Orders}/[U]>>safe(Orders,U), Updates, Unsafe),
    maplist({Orders}/[U,NewU]>>change_unsafe(Orders,U,NewU), Unsafe, NewUpdates),
    maplist(middle_number, NewUpdates, Mids),
    sum(Mids, #=, K).
