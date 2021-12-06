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

%-------------------------------------------------%
%         Intersection Task -- Lines Utils        %
%-------------------------------------------------%

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

% get_range_list_2(+Coord1, +Coord2, -ListOfCoords)
get_range_list_2(X1, X2, XList) :-
    (X1 < X2 ->
        bagof(N, between(X1, X2, N), XList);
        bagof(N, between(X2, X1, N), Xs),
        reverse(Xs, XList)
    ).

% line_cover(+Line, -ListOfPoints)
line_cover([(X1, Y), (X2, Y)], Points) :-
    get_range_list(X1, X2, XList), !,
    maplist(Y+\X^(create_tuple(X, Y)), XList, Points).
line_cover([(X, Y1), (X, Y2)], Points) :-
    get_range_list(Y1, Y2, YList), !,
    maplist(X+\Y^(create_tuple(X, Y)), YList, Points).

% line_cover_2(+Line, -ListOfPoints)
line_cover_2([(X1, Y1), (X2, Y2)], Points) :-
    get_range_list_2(X1, X2, XList),
    get_range_list_2(Y1, Y2, YList),
    zip(XList, YList, Points).

%-------------------------------------------------%
%         Intersection Task -- Trie Utils         %
%-------------------------------------------------%

% update_count_trie(+Trie, +Point)
update_count_trie(T, Point) :-
    (trie_lookup(T, Point, C) ->
        NC #= C + 1;
        NC = 1
    ),
    trie_update(T, Point, NC).

% update_count_line(+Trie, +Line)
update_count_line(T, L) :-
    line_cover(L, CoverPoints), !,
    maplist(update_count_trie(T), CoverPoints).

% update_count_line_2(+Trie, +Line)
update_count_line_2(T, L) :-
    line_cover_2(L, CoverPoints),
    maplist(update_count_trie(T), CoverPoints).

% is_intersection(+Trie, +Point)
is_intersection(T, Point) :-
    trie_lookup(T, Point, C), C #>= 2, !.

% count_intersection_points(+Trie, -Count)
count_intersection_points(T, Count) :-
    get_trie_keys(T, Keys), !,
    include(is_intersection(T), Keys, Points),
    length(Points, Count).

task_1(Out) :-
    get_lines(Lines), !, trie_new(T), !,
    include(\L^(horizontal_line(L);vertical_line(L)), Lines, FLines),
    maplist(update_count_line(T), FLines),
    count_intersection_points(T, Out).

task_2(Out) :-
    get_lines(Lines), !, trie_new(T), !,
    include(\L^(horizontal_line(L);vertical_line(L)), Lines, FLines1),
    maplist(update_count_line(T), FLines1), !,
    exclude(\L^(horizontal_line(L);vertical_line(L)), Lines, FLines2),
    maplist(update_count_line_2(T), FLines2), !,
    count_intersection_points(T, Out).
