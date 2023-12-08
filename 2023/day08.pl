% nvim: set syntax=prolog

% AoC 2023 Day 8

:- use_module(library(clpfd)).
:- consult('../helper.pl').

% pre-parsed input
:- consult('input_08').

% instruction_chars(-InstrChar)
instruction_chars(InstrChar) :-
    instruction_str(InstrStr),
    string_chars(InstrStr, InstrChar).

% get_curr_intruction(+InstrCount, -Instr)
get_curr_intruction(InstrCount, Instr) :-
    instruction_chars(InstrChar),
    length(InstrChar, TotalInstr),
    Index #= InstrCount mod TotalInstr,
    nth0(Index, InstrChar, Instr).

% travel_steps_1(+CurrNode, +InstrCount, -TotalSteps)
travel_steps_1(zzz, InstrCount, InstrCount).
travel_steps_1(Curr, InstrCount, TotalSteps) :-
    get_curr_intruction(InstrCount, 'L'),
    connection(Curr, NextNode, _),
    NewInstrCount #= InstrCount + 1,
    travel_steps_1(NextNode, NewInstrCount, TotalSteps).
travel_steps_1(Curr, InstrCount, TotalSteps) :-
    get_curr_intruction(InstrCount, 'R'),
    connection(Curr, _, NextNode),
    NewInstrCount #= InstrCount + 1,
    travel_steps_1(NextNode, NewInstrCount, TotalSteps).

task_1(TotalSteps) :-
    travel_steps_1(aaa, 0, TotalSteps).

% is_a_node(+Node)
is_a_node(Node) :-
    atom_chars(Node, [_, _, a]).
is_z_node(Node) :-
    atom_chars(Node, [_, _, z]).

% travel_steps_2(+InstrCount, +CurrNode, -TotalSteps)
travel_steps_2(InstrCount, CurrNode, InstrCount) :-
    is_z_node(CurrNode).
travel_steps_2(InstrCount, Curr, TotalSteps) :-
    get_curr_intruction(InstrCount, 'L'),
    connection(Curr, NextNode, _),
    NewInstrCount #= InstrCount + 1,
    travel_steps_2(NewInstrCount, NextNode, TotalSteps).
travel_steps_2(InstrCount, Curr, TotalSteps) :-
    get_curr_intruction(InstrCount, 'R'),
    connection(Curr, _, NextNode),
    NewInstrCount #= InstrCount + 1,
    travel_steps_2(NewInstrCount, NextNode, TotalSteps).

% all_a_nodes(-ANodes)
all_a_nodes(ANodes) :-
    findall(Node, connection(Node, _, _), AllNodes),
    include(is_a_node, AllNodes, ANodes).

% Thank you swipl for your built-in lcm <3
lcm_(A, B, LCM) :- LCM is lcm(A, B).

task_2(TotalSteps) :-
    all_a_nodes(ANodes),
    maplist(travel_steps_2(0), ANodes, TotalStepsList),
    foldl(lcm_, TotalStepsList, 1, TotalSteps).
