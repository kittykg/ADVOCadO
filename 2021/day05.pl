% nvim: set syntax=prolog

% AoC 2021 Day 5

:- consult('../helper.pl').
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).


% Point: (Int, Int)
% Line:  [Point, Point]

%-----------------------------------%
%            IO parsing             %
%-----------------------------------%


% parse_string_to_lines(+String, -Line)
parse_string_to_lines(S, [(X1, Y1), (X2, Y2)]) :-
    split_string(S, " ", "", L),
    exclude(\X^(X == "->"), L, NewL),
    maplist(parse_string_to_point, NewL, [(X1, Y1), (X2, Y2)]).

% parse_string_to_point(+String, -Point)
parse_string_to_point(S, (X, Y)) :-
    split_string(S, ",", "", L),
    maplist(atom_number, L, [X, Y]).

% get_lines(-ListOfLines)
get_lines(Lines) :-
    open('input_05', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(parse_string_to_lines, SL, Lines).

%-----------------------------------%
%         Intersection Task         %
%-----------------------------------%

% horizntal_line(+Line), vertical_line(+Line)
horizontal_line([(_, Y), (_, Y)]).
vertical_line([(X, _), (X, _)]).

% create_tuple(+XCoord, +YCoord, -Point)
create_tuple(X, Y, (X, Y)).

% line_cover(+Line, -ListOfPoints)
line_cover([(X1, Y), (X2, Y)], Points) :-
    (X1 < X2 ->
        LX = X1, UX = X2;
        LX = X2, UX = X1
    ), !,
    bagof(N, between(LX, UX, N), XList),
    maplist(Y+\X^(create_tuple(X, Y)), XList, Points).

line_cover([(X, Y1), (X, Y2)], Points) :-
    (Y1 < Y2 ->
        LY = Y1, UY = Y2;
        LY = Y2, UY = Y1
    ), !,
    bagof(N, between(LY, UY, N), YList),
    maplist(X+\Y^(create_tuple(X, Y)), YList, Points).

% append_line_cover(+Line, +ListOfPoints, -NewListOfPoints)
append_line_cover([P1, P2], PL, NewPL) :-
    line_cover([P1, P2], Points),
    append(Points, PL, NewPL).

% count_duplicates(+List, +SeenElements, +KnownDuplicates, -Count)
count_duplicates([], _, _, 0).
count_duplicates([X|R], SeenSet, Duplicates, N) :-
    member(X, SeenSet), !,
    (member(X, Duplicates) ->
        count_duplicates(R, SeenSet, Duplicates, N), !;
        count_duplicates(R, SeenSet, [X|Duplicates], C), !,
        N #= C + 1
    ).
count_duplicates([X|R], SeenSet, Duplicates, N) :-
    count_duplicates(R, [X|SeenSet], Duplicates, N).

task_1(Out) :-
    get_lines(Lines),
    include(\L^(horizontal_line(L);vertical_line(L)), Lines, FLines),
    foldl(append_line_cover, FLines, [], AllPoints), !,
    sort(0, @=<, AllPoints, SortedP), !,
    count_duplicates(SortedP, [], [], Out).

% get_range_list(+Coord1, +Coord2, -ListOfCoords)
get_range_list(X1, X2, XList) :-
    (X1 < X2 ->
        bagof(N, between(X1, X2, N), XList);
        bagof(N, between(X2, X1, N), Xs),
        reverse(Xs, XList)
    ).

% line_cover_2(+Line, -ListOfPoints)
line_cover_2([(X1, Y1), (X2, Y2)], Points) :-
    get_range_list(X1, X2, XList),
    get_range_list(Y1, Y2, YList),
    zip(XList, YList, Points).

% append_line_cover_2(+Line, +ListOfPoints, -ListOfNewPoints)
append_line_cover_2([P1, P2], PL, NewPL) :-
    line_cover_2([P1, P2], Points),
    append(Points, PL, NewPL).

task_2(Out) :-
    get_lines(Lines),
    include(\L^(horizontal_line(L);vertical_line(L)), Lines, FLines),
    foldl(append_line_cover, FLines, [], AllPoints1), !,
    exclude(\L^(horizontal_line(L);vertical_line(L)), Lines, HorLines),
    foldl(append_line_cover_2, HorLines, [], AllPoints2), !,
    append(AllPoints1, AllPoints2, AllPoints),
    sort(0, @=<, AllPoints, SortedP), !,
    count_duplicates(SortedP, [], [], Out).

