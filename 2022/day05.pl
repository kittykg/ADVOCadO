% nvim: set syntax=prolog

% AoC 2022 Day 5
% Note: Input instructions are parsed into form Num1,Num2,Num3 per line.
%       Input crates config are parsed into String per line.

:- consult('../helper.pl').

% parse_instruction_line(+Line, -ListOfNumber)
parse_instruction_line(L, NL) :-
    split_string(L, ",", "", SL),
    maplist(string_to_number, SL, NL).

% get_instructions(-Instructions)
get_instructions(I) :-
    open('input_05', read, Stream),
    read_string_file(Stream, IL),
    maplist(parse_instruction_line, IL, I).

% get_crates(-ListOfCodes)
get_crates(L) :-
    open('input_05_crate', read, Stream),
    read_string_file(Stream, SL),
    maplist(string_codes, SL, L).

% move(+RetainOrder, +Instruction, +Crates, -NewCrates)
move(RetainOrder, [NCrates, Source, Dest], Crates, NewCrates) :-
    nth0(Source, Crates, SC),
    append(NewSource, S, SC),
    length(S, NCrates), !,
    (RetainOrder -> RS = S ; reverse(S, RS)),
    nth0(Dest, Crates, DC),
    append(DC, RS, NewDest),
    select(SC, Crates, NewSource, Temp),
    select(DC, Temp, NewDest, NewCrates).

% get_output_string(+Crates, -String)
get_output_string(Crates, S) :-
    maplist(last, Crates, SCode),
    string_codes(S, SCode).

task_1(S) :-
    get_crates(C),
    get_instructions(I),
    foldl(move(false), I, C, FinalC),
    get_output_string(FinalC, S).

task_2(S) :-
    get_crates(C),
    get_instructions(I),
    foldl(move(true), I, C, FinalC),
    get_output_string(FinalC, S).
