% nvim: set syntax=prolog

% AoC 2023 Day 12 WIP

:- use_module(library(clpfd)).
:- consult('../helper.pl').

% pre-parsed input.
% :- consult('input_12').

% % turning_point(+Prev, +Curr, +Next)
% turning_point(n, d, _).
% turning_point(o, d, _).
% turning_point(_, d, o).
% turning_point(_, d, n).

% find_turning_points_([_, n], _, []).
% find_turning_points_([H1, H2, H3|R], I, [I|TurningPoints]) :-
%     turning_point(H1, H2, H3),
%     NewI #= I + 1,
%     find_turning_points_([H2, H3|R], NewI, TurningPoints).
% find_turning_points_([_, H2, H3|R], I, TurningPoints) :-
%     NewI #= I + 1,
%     find_turning_points_([H2, H3|R], NewI, TurningPoints).

% find_turning_points(HotSpring, TurningPoints) :-
%     append([n|HotSpring], [n], HotSpringWithN),
%     find_turning_points_(HotSpringWithN, 0, TurningPoints).


% #....######..#####. 1,6,5

contiguous_group(Len, Group) :-
    repeat('#', Len, Group).
working_group(Len, Group) :-
    repeat('.', Len, Group).

join_contiguous_groups_string([CGS], Acc, FinalString) :-
    string_concat(Acc, CGS, FinalString).
join_contiguous_groups_string([CGS|CGSL], Acc, FinalString) :-
    working_group(_, WG),
    string_concat(Acc, CGS, Acc2),
    string_concat(Acc2, WG, Acc3),
    join_contiguous_groups_string(CGSL, Acc3, FinalString).

concat_contiguous_groups_string(CGL, Str) :-
    maplist([In, Out]>>string_chars(Out, In), CGL, CGLS),
    join_contiguous_groups_string(CGLS, "", Str).



% hotspring_substring() :-
match_contiguous_group(HotSpring, ContiguousGroups, Combination) :-
    maplist(contiguous_group, ContiguousGroups, CGL),
    concat_contiguous_groups_string(CGL, Combination),
    sub_string(HotSpring, _, _, _, Combination).


