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

% get_range_list(+Coord1, +Coord2, -ListOfCoords)
get_range_list(X1, X2, XList) :-
    (X1 < X2 ->
        bagof(N, between(X1, X2, N), XList);
        bagof(N, between(X2, X1, N), XList)
    ).

% line_cover(+Line, -ListOfPoints)
line_cover([(X1, Y), (X2, Y)], Points) :-
    get_range_list(X1, X2, XList), !,
    maplist(Y+\X^(create_tuple(X, Y)), XList, Points).
line_cover([(X, Y1), (X, Y2)], Points) :-
    get_range_list(Y1, Y2, YList), !,
    maplist(X+\Y^(create_tuple(X, Y)), YList, Points).

% point_to_atom(+Point, -Atom)
point_to_atom((X, Y), A) :-
    atomics_to_string([x, X, y, Y], S),
    atom_string(A, S).

% update_count_dict(+Point, +Dict, -NewDict)
update_count_dict(Point, Dict, NewDict) :-
    point_to_atom(Point, PA), !,
    (in_dict(Dict, PA), ! ->
        C = Dict.get(PA), NC #= C + 1;
        NC = 1
    ),
    NewDict = Dict.put(PA, NC).

% is_intersection(+Dict, +Key)
is_intersection(Dict, K) :-
    V = Dict.get(K),
    V #>= 2.

% count_intersection_points(+Dict, -Count)
count_intersection_points(Dict, Count) :-
    dict_keys(Dict, Keys),
    include(is_intersection(Dict), Keys, Points), !,
    length(Points, Count).

% update_count_line(+Line, +Dict, -NewDict)
update_count_line(L, Dict, NewDict) :-
    line_cover(L, CoverPoints), !,
    foldl(update_count_dict, CoverPoints, Dict, NewDict).

task_1(Out) :-
    get_lines(Lines), !,
    include(\L^(horizontal_line(L);vertical_line(L)), Lines, FLines), !,
    foldl(update_count_line, FLines, pc{}, OutDict), !,
    count_intersection_points(OutDict, Out).


% get_range_list_2(+Coord1, +Coord2, -ListOfCoords)
get_range_list_2(X1, X2, XList) :-
    (X1 < X2 ->
        bagof(N, between(X1, X2, N), XList);
        bagof(N, between(X2, X1, N), Xs),
        reverse(Xs, XList)
    ).

% line_cover_2(+Line, -ListOfPoints)
line_cover_2([(X1, Y1), (X2, Y2)], Points) :-
    get_range_list_2(X1, X2, XList),
    get_range_list_2(Y1, Y2, YList),
    zip(XList, YList, Points).

% update_count_line(+Line, +Dict, -NewDict)
update_count_line_2(L, Dict, NewDict) :-
    line_cover_2(L, CoverPoints),
    foldl(update_count_dict, CoverPoints, Dict, NewDict).

task_2(Out) :-
    get_lines(Lines),
    include(\L^(horizontal_line(L);vertical_line(L)), Lines, FLines), !,
    foldl(update_count_line, FLines, pc{}, OutDict1), !,
    exclude(\L^(horizontal_line(L);vertical_line(L)), Lines, HorLines),
    foldl(update_count_line_2, HorLines, OutDict1, OutDict), !,
    count_intersection_points(OutDict, Out).
