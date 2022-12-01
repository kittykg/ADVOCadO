% nvim: set syntax=prolog

% AoC 2022 Day 1

:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).


% DCG grammar for a line
lines([]) --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

eos([], []).

line([]) --> ("\n"; call(eos)), !.
line([L|LS]) --> [L], line(LS).

% elf_group(+List, +AccumulateList, -Sums)
elf_group([], AccL, [Sum]) :-
    sum_list(AccL, Sum).
elf_group([e|L], AccL, [Sum|Elfs]) :-
    sum_list(AccL, Sum),
    elf_group(L, [], Elfs).
elf_group([X|L], AccL, Elfs) :-
    elf_group(L, [X|AccL], Elfs).

% code_to_number_or_empty(+List, -NumberOrEmpty)
code_to_number_or_empty([], e).
code_to_number_or_empty(C, N) :-
    is_list(C),
    number_codes(N, C).

% get_elf_group(-Sums)
get_elf_group(AllSums) :-
    phrase_from_file(lines(Ls), 'input_01'),
    maplist(code_to_number_or_empty, Ls, ElfInput),
    elf_group(ElfInput, [], AllSums).

task_1(Max) :-
    get_elf_group(AllSums),
    max_list(AllSums, Max).

task_2(SumMax3) :-
    get_elf_group(AllSums),
    sort(0, @>, AllSums, [A,B,C|_]),
    SumMax3 #= A + B + C.
