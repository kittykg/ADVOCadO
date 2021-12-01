% nvim :set syntax=prolog

% AoC 2020 Day 8

:- consult('../helper.pl').

string_to_instr(S, (Op, N)) :-
    split_string(S, " ", "", [OpStr, NStr]),
    term_string(Op, OpStr),
    number_string(N, NStr).

parse(L, Instrs) :- maplist(string_to_instr, L, Instrs).

exec_instr((nop, _), Pt, Acc, NextPt, Acc) :- NextPt is Pt + 1.
exec_instr((jmp, N), Pt, Acc, NextPt, Acc) :- NextPt is Pt + N.
exec_instr((acc, N), Pt, Acc, NextPt, NextAcc) :-
    NextPt is Pt + 1, NextAcc is Acc + N.

task_1_exec(_, Pt, Acc, Visited, Acc) :- member(Pt, Visited).
task_1_exec(L, Pt, Acc, Visited, Res) :-
    nth0(Pt, L, Ins),
    exec_instr(Ins, Pt, Acc, NextPt, NextAcc),
    task_1_exec(L, NextPt, NextAcc, [Pt|Visited], Res).

task_1(N) :-
    open('input_08', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream),
    parse(L, Instrs),
    task_1_exec(Instrs, 0, 0, [], N).

task_2_exec(_, Pt, Acc, Visited, Acc, loop) :- member(Pt, Visited).
task_2_exec(L, Pt, Acc, Visited, Acc, terminate) :-
    \+ member(Pt, Visited),
    length(L, Pt).
task_2_exec(L, Pt, Acc, Visited, Res, Status) :-
    nth0(Pt, L, Ins),
    exec_instr(Ins, Pt, Acc, NextPt, NextAcc),
    task_2_exec(L, NextPt, NextAcc, [Pt|Visited], Res, Status).

is_changeable((nop, _)).
is_changeable((jmp, _)).

change_instr((nop, N), (jmp, N)).
change_instr((jmp, N), (nop, N)).

modify_instr(Pos, Instrs, NewInstrs) :-
    split_at(Pos, Instrs, Sub1, [OldInstr|Sub2]),
    change_instr(OldInstr, NewInstr),
    append(Sub1, [NewInstr|Sub2], NewInstrs).

task_2_(Instrs, Pos, Res) :-
    length(Instrs, Len), Pos < Len,
    nth0(Pos, Instrs, CurrInstr),
    (is_changeable(CurrInstr) ->
        modify_instr(Pos, Instrs, ModInstrs),
        task_2_exec(ModInstrs, 0, 0, [], ResAcc , Status),
        ( Status == terminate ->
            Res is ResAcc,! ;
            NextPos is Pos + 1,
            task_2_(Instrs, NextPos, Res)
        );
        NextPos is Pos + 1,
        task_2_(Instrs, NextPos, Res)
    ).

task_2(N) :-
    open('input_08', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream),
    parse(L, Instrs),
    task_2_(Instrs, 0, N).
