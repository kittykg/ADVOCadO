% nvim: set syntax=prolog

% AoC 2021 Day 13

:- consult('../helper.pl').
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).

% Coord: (Int, Int)
% FoldInstr: (1{x,y}1, Int)

%-----------------------------------%
%            IO parsing             %
%-----------------------------------%

% parse_string_to_ints(+String, -Coord)
parse_string_to_coord(S, (X, Y)) :-
    split_string(S, ",", "", SL),
    maplist(number_string_, SL, [X, Y]).

number_string_(String, Number) :-
    number_string(Number, String).

% get_paper(-ListOfCoords)
get_paper(Coords) :-
    open('input_13_paper', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(parse_string_to_coord, SL, Coords).

% parse_fold_instr(+String, -FoldInstr)
parse_fold_instr(S, (Axis, Val)) :-
    split_string(S, " ", "", [_, _, Instr]),
    split_string(Instr, "=", "", [AxisString, ValString]),
    atom_string(Axis, AxisString),
    number_string(Val, ValString).

% get_fold_instr(-ListOfFoldInstrs)
get_fold_instr(FoldInstrs) :-
    open('input_13_fold', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(parse_fold_instr, SL, FoldInstrs).

%-----------------------------------%
%           Origami Task            %
%-----------------------------------%

%mirror(+FoldInstr, +Coord, -MirroredCoord)
mirror((x, XVal), (X, Y), (NewX, Y)) :- NewX #= XVal - X + XVal.
mirror((y, YVal), (X, Y), (X, NewY)) :- NewY #= YVal - Y + YVal.

% get_section(+FoldInstr, +ListOfCoords, -Section1Coords, -Section2Coords)
get_section(FoldInstr, Coords, Section1, Section2) :-
    include(filter_coord(FoldInstr), Coords, Section1),
    exclude(filter_coord(FoldInstr), Coords, Section2).

% filter_coord(+FoldInstr, +Coord)
filter_coord((x, XVal), (X, _)) :- X #< XVal, !.
filter_coord((y, YVal), (_, Y)) :- Y #< YVal, !.

% fold_once(+FoldInstr, +Coords, +CoordsAfterFold)
fold_once(FoldInstr, Coords, NewCoords) :-
    get_section(FoldInstr, Coords, Section1, Section2),
    maplist(mirror(FoldInstr), Section2, MirroredCoords),
    append(Section1, MirroredCoords, AllCoords),
    sort(0, @<, AllCoords, NewCoords).

task_1(Out) :-
    get_paper(Coords), get_fold_instr([FoldInstr|_]), !,
    fold_once(FoldInstr, Coords, NewCoords),
    length(NewCoords, Out).

% board_visualisation(+Coords)
board_visualisation(Coords) :-
    unzip(Coords, Xs, Ys),
    max_list(Xs, MaxX), max_list(Ys, MaxY), !,
    board_trie(Coords, Trie),
    bagof(N, between(0, MaxY, N), YList),
    maplist(draw_board_row(MaxX, Trie), YList).

board_trie(Coords, Trie) :-
    trie_new(Trie), maplist(board_trie_update_(Trie), Coords).
board_trie_update_(Trie, Coord) :- trie_insert(Trie, Coord, '#').

draw_board_row(MaxX, Trie, Y) :-
    bagof(N, between(0, MaxX, N), XList),
    maplist(\X^(draw_board_coord(Y, X, Trie)), XList, Chars),
    string_chars(RowString, Chars),
    writeln(RowString).

draw_board_coord(Y, X, Trie, Char) :-
    (trie_gen(Trie, (X, Y)), ! ->
        trie_lookup(Trie, (X, Y), Char);
        Char = '.'
    ).

% fold_all(+FoldInstrs, +InitialCoords, -FinalCoords)
fold_all([], Coords, Coords).
fold_all([FoldInstr|R], Coords, FinalCoords) :-
    fold_once(FoldInstr, Coords, NewCoords),
    fold_all(R, NewCoords, FinalCoords).

task_2 :-
    get_paper(Coords), get_fold_instr(FoldInstrs), !,
    fold_all(FoldInstrs, Coords, FinalCoords),
    board_visualisation(FinalCoords).
