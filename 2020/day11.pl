% nvim :set syntax=prolog

% AoC 2020 Day 11

:- consult('../helper.pl').

get_seat(_, CoordX, CoordY, '.') :- (CoordX < 0 ; CoordY < 0).
get_seat([X|L], CoordX, CoordY, '.') :-
    length([X|L], NumRows), length(X, NumCols),
    (CoordX >= NumRows ; CoordY >= NumCols).
get_seat(L, CoordX, CoordY, S) :-
    nth0(CoordX, L, Row), nth0(CoordY, Row, S).

count_adjacent(CoordX, CoordY, L, NumOcc) :-
    XM1 is CoordX - 1, XP1 is CoordX + 1,
    Xs = [XM1, XM1, XM1, CoordX, CoordX, XP1, XP1, XP1],
    YM1 is CoordY - 1, YP1 is CoordY + 1,
    Ys = [YM1, CoordY, YP1, YM1, YP1, YM1, CoordY, YP1],
    maplist(get_seat(L), Xs, Ys, Res),
    count('#', Res, NumOcc).

new_status(L, CoordX, CoordY, '#') :-
    get_seat(L, CoordX, CoordY, 'L'),
    count_adjacent(CoordX, CoordY, L, 0).
new_status(L, CoordX, CoordY, 'L') :-
    get_seat(L, CoordX, CoordY, '#'),
    count_adjacent(CoordX, CoordY, L, NumOcc),
    NumOcc >= 4.
new_status(L, CoordX, CoordY, Status) :- get_seat(L, CoordX, CoordY, Status).

one_row(Row, CoordX, L, NewRow) :-
    length(Row, NumCol), YRange is NumCol - 1, numlist(0, YRange, Ys),
    maplist(new_status(L, CoordX), Ys, NewRow).

one_iter([], _, _, []).
one_iter([Row|Rest], CoordX, L, [NewRow|NewRest]) :-
    one_row(Row, CoordX, L, NewRow),
    NextCoordX is CoordX + 1,
    one_iter(Rest, NextCoordX, L, NewRest).

task_1_(L, N) :-
    one_iter(L, 0, L, NewL), !,
    ( L == NewL ->
        maplist(count('#'), L, RowCounts),
        sum_list(RowCounts, N);
        task_1_(NewL, N)
    ).

% -------------------------------------------

direction_to_coord(0, X, Y, CX, CY) :- CX is X - 1, CY is Y - 1.
direction_to_coord(1, X, Y, CX, CY) :- CX is X - 1, CY is Y.
direction_to_coord(2, X, Y, CX, CY) :- CX is X - 1, CY is Y + 1.
direction_to_coord(3, X, Y, CX, CY) :- CX is X, CY is Y - 1.
direction_to_coord(4, X, Y, CX, CY) :- CX is X, CY is Y + 1.
direction_to_coord(5, X, Y, CX, CY) :- CX is X + 1, CY is Y - 1.
direction_to_coord(6, X, Y, CX, CY) :- CX is X + 1, CY is Y.
direction_to_coord(7, X, Y, CX, CY) :- CX is X + 1, CY is Y + 1.

in_bound(X, Y, L) :-
    length(L, NumRows), X >= 0, X < NumRows, !,
    nth0(0, L, Col), length(Col, NumCols),
    Y >= 0, Y < NumCols, !.

occupied_seat_on_line(CoordX, CoordY, L, Direction) :-
    direction_to_coord(Direction, CoordX, CoordY, CX, CY),
    in_bound(CX, CY, L), get_seat(L, CX, CY, '#').
occupied_seat_on_line(CoordX, CoordY, L, Direction) :-
    direction_to_coord(Direction, CoordX, CoordY, CX, CY),
    in_bound(CX, CY, L), get_seat(L, CX, CY, '.'),
    occupied_seat_on_line(CX, CY, L, Direction).

count_adjacent_line(CoordX, CoordY, L, NumOcc) :-
    numlist(0, 7, Directions),
    include(occupied_seat_on_line(CoordX, CoordY, L), Directions, Included),
    length(Included, NumOcc).

new_status_2(L, CoordX, CoordY, '#') :-
    get_seat(L, CoordX, CoordY, 'L'),
    count_adjacent_line(CoordX, CoordY, L, 0).
new_status_2(L, CoordX, CoordY, 'L') :-
    get_seat(L, CoordX, CoordY, '#'),
    count_adjacent_line(CoordX, CoordY, L,NumOcc),
    NumOcc >= 5.
new_status_2(L, CoordX, CoordY, Status) :-
    get_seat(L, CoordX, CoordY, Status).

one_row_2(Row, CoordX, L, NewRow) :-
    length(Row, NumCol), YRange is NumCol - 1, numlist(0, YRange, Ys),
    maplist(new_status_2(L, CoordX), Ys, NewRow).

one_iter_2([], _, _, []).
one_iter_2([Row|Rest], CoordX, L, [NewRow|NewRest]) :-
    one_row_2(Row, CoordX, L, NewRow),
    NextCoordX is CoordX + 1,
    one_iter_2(Rest, NextCoordX, L, NewRest).

task_2_(L, N) :-
    one_iter_2(L, 0, L, NewL), !,
    ( L == NewL ->
        maplist(count('#'), L, RowCounts),
        sum_list(RowCounts, N);
        task_2_(NewL, N)
    ).

% -------------------------------------------

get_input(L) :-
    open('input_11', read, Stream),
    read_string_file(Stream, L1), !,
    close(Stream),
    maplist(string_chars, L1, L).

task_1(N) :-
    get_input(L), task_1_(L, N).

task_2(N) :-
    get_input(L), task_2_(L, N).
