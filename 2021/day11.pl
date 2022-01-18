% nvim: set syntax=prolog

% AoC 2021 Day 11

:- consult('../helper.pl').
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).

% Grid: [[Int]]

%-----------------------------------%
%            IO parsing             %
%-----------------------------------%

% parse_string_to_ints(+String, -Ints)
parse_string_to_ints(S, L) :-
    string_chars(S, Chars),
    maplist(atom_number, Chars, L).

% get_initial_grid(-Grid)
get_initial_grid(Grid) :-
    open('input_11', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(parse_string_to_ints, SL, Grid).

% make_tuple(+XCoord, +YCoord, -Coord)
make_tuple(X, Y, (X, Y)).

% get_point(+Coord, +Grid, -Point)
get_point((X, Y), G, Point) :-
    nth0(Y, G, XRow),
    nth0(X, XRow, Point).

grid_trie_insert(X, Y, G, Trie) :-
    make_tuple(X, Y, Coord),
    get_point(Coord, G, Value),
    trie_insert(Trie, Coord, Value).

inital_grid_trie_row(Y, G, Trie) :-
    nb_getval(maxX, MaxX),
    EndX #= MaxX - 1,
    bagof(N, between(0, EndX, N), XList),
    maplist(\X^(grid_trie_insert(X, Y, G, Trie)), XList).

initial_grid_trie(Trie) :-
    nb_getval(maxY, MaxY),
    EndY #= MaxY - 1,
    bagof(N, between(0, EndY, N), YList),
    trie_new(Trie),
    get_initial_grid(G),
    maplist(\Y^(inital_grid_trie_row(Y, G, Trie)), YList).

% in_bound(+Coord)
in_bound((X, Y)) :-
    nb_getval(maxX, MaxX), nb_getval(maxY, MaxY),
    X #< MaxX, X #> -1, Y #< MaxY, Y #> -1, !.

% get_neighbours_coord(+Coord, +Grid, -NeighboursCoords)
get_neighbours_coord((X, Y), Neighbours) :-
    LeftX #= X - 1,
    RightX #= X + 1,
    TopY #= Y - 1,
    BotY #= Y + 1,
    include(in_bound, [
        (LeftX, TopY), (X, TopY), (RightX, TopY),
        (LeftX, Y), (RightX, Y),
        (LeftX, BotY), (X, BotY), (RightX, BotY)
    ], Neighbours).

progress_one_step(Trie, Count) :-
    nb_getval(maxY, MaxY),
    EndY #= MaxY - 1,
    bagof(N, between(0, EndY, N), YList),
    maplist(progress_one_step_(Trie), YList),
    get_trie_keys(Trie, AllCoords),
    include(blink(Trie), AllCoords, BlinkCoords),
    length(BlinkCoords, Count),
    maplist(\X^(update_blink_(Trie, X)), BlinkCoords).

progress_one_step_(Trie, Y) :-
    nb_getval(maxX, MaxX),
    EndX #= MaxX - 1,
    bagof(N, between(0, EndX, N), XList),
    maplist(\X^(update_one_(Trie, (X, Y))), XList).

update_one_(Trie, Coord) :-
    increase_one_(Trie, Coord),
    (blink(Trie, Coord), ! ->
        writeln(Coord),
        get_neighbours_coord(Coord, Neighbours),
        maplist(update_one_(Trie), Neighbours);
        true
    ).

increase_one_(Trie, Coord) :-
    trie_lookup(Trie, Coord, Value),
    NewVal #= Value + 1,
    trie_update(Trie, Coord, NewVal).

update_blink_(Trie, Coord) :- trie_update(Trie, Coord, 0).

blink(Trie, Coord) :-
    trie_lookup(Trie, Coord, Value),
    Value #> 9, !.

print_grid(Trie) :-
    bagof(N, between(0, 9, N), YList),
    maplist(print_grid_(Trie), YList).

print_grid_(Trie, Y) :-
    bagof(N, between(0, 9, N), XList),
    maplist(\X^(trie_lookup(Trie, (X, Y))), XList, Values),
    atomics_to_string(Values, S),
    writeln(S).
