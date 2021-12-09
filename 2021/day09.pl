% nvim: set syntax=prolog

% AoC 2021 Day 4

:- consult('../helper.pl').
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).


%-----------------------------------%
%            IO parsing             %
%-----------------------------------%

parse_string_to_ints(S, L) :-
    string_chars(S, Chars),
    maplist(atom_number, Chars, L).

get_board(Board) :-
    open('input_09', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(parse_string_to_ints, SL, Board).

%-----------------------------------%
%            Basin Task             %
%-----------------------------------%

get_point((X, Y), L, Point) :-
    nth0(Y, L, XRow),
    nth0(X, XRow, Point).

make_tuple(X, Y, (X, Y)).

in_bound((MaxX, MaxY), (X, Y)) :-
    X #< MaxX, X #> -1, Y #< MaxY, Y #> -1, !.

get_neighbours_coord((X, Y), L, Neighbours) :-
    length(L, MaxY),
    nth0(0, L, Row),
    length(Row, MaxX),
    LeftX #= X - 1,
    RightX #= X + 1,
    TopY #= Y - 1,
    BotY #= Y + 1,
    include(in_bound((MaxX, MaxY)), [
        (LeftX, Y), (RightX, Y), (X, TopY), (X, BotY)
    ], Neighbours).

get_neighbours_point((X, Y), L, NBPoints) :-
    get_neighbours_coord((X, Y), L, NeighbourCoords),
    maplist(\C^(get_point(C, L)), NeighbourCoords, NBPoints).

low_point((X, Y), L) :-
    get_point((X, Y), L, Point),
    get_neighbours_point((X, Y), L, NBPoints),
    include(lower_(Point), NBPoints, NBPoints).

lower_(P1, P2) :- P1 #< P2.

get_low_point_coord_in_row(Y, L, AllLowPoints) :-
    nth0(Y, L, Row),
    length(Row, MaxX),
    EndX #= MaxX - 1,
    bagof(N, between(0, EndX, N), XList),
    include(\X^(low_point((X, Y), L)), XList, LowPointX),
    maplist(\X^(make_tuple(X, Y)), LowPointX, AllLowPoints).

get_all_low_point_coord(B, AllCoord) :-
    length(B, MaxY),
    EndY #= MaxY - 1,
    bagof(N, between(0, EndY, N), YList),
    maplist(\Y^(get_low_point_coord_in_row(Y, B)), YList, AllPointsList),
    flatten(AllPointsList, AllCoord).

task_1(Out) :-
    get_board(B),
    get_all_low_point_coord(B, AllCoord),
    maplist(\C^(get_point(C, B)), AllCoord, AllPoints),
    sumlist(AllPoints, SumPoints),
    length(AllPoints, Extra),
    Out #= SumPoints + Extra.

basin((X, Y), B, 0) :-
    get_point((X, Y), B, 9).
basin(Coord, _, 0) :-
    nb_getval(seen, Seen), member(Coord, Seen).
basin((X, Y), Board, Count) :-
    get_neighbours_coord((X, Y), Board, Neighbours),
    nb_getval(seen, Seen),
    nb_setval(seen, [(X, Y)|Seen]),
    maplist(\C^(basin(C, Board)), Neighbours, Counts),
    sumlist(Counts, SumCounts),
    Count #= SumCounts + 1.

task_2(Out) :-
    get_board(B),
    get_all_low_point_coord(B, AP),
    nb_setval(seen, []),
    maplist(\C^(basin(C, B)), AP, BasinSize),
    sort(0, @>=, BasinSize, [S1, S2, S3|_]),
    Out #= S1 * S2 * S3.
