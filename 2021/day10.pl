% nvim: set syntax=prolog

% AoC 2021 Day 10

:- consult('../helper.pl').
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).

%-----------------------------------%
%            IO parsing             %
%-----------------------------------%

% get_lines(-Lines)
get_lines(Lines) :-
    open('input_10', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(string_chars, SL, Lines).

%-----------------------------------%
%           Syntax Task             %
%-----------------------------------%

left_bracket('(').
left_bracket('[').
left_bracket('{').
left_bracket('<').

right_bracket(')').
right_bracket(']').
right_bracket('}').
right_bracket('>').

pair_brackets('(', ')').
pair_brackets('[', ']').
pair_brackets('{', '}').
pair_brackets('<', '>').

error_score(')', 3).
error_score(']', 57).
error_score('}', 1197).
error_score('>', 25137).

autocomplete_score(')', 1).
autocomplete_score(']', 2).
autocomplete_score('}', 3).
autocomplete_score('>', 4).

% parse_line(+Line, +Stack, -Msg)
parse_line([], [], correct).
parse_line([], Stack, incomplete) :-
    length(Stack, Len),
    Len #> 0.
parse_line([H|R], Stack, Msg) :-
    left_bracket(H),
    parse_line(R, [H|Stack], Msg).
parse_line([H|R], [S|RS], Msg) :-
    right_bracket(H),
    pair_brackets(S, H),
    parse_line(R, RS, Msg).
parse_line([H|_], _, H) :-
    right_bracket(H).

task_1(Out) :-
    get_lines(Lines),
    maplist(\X^(parse_line(X, [])), Lines, Msgs),
    exclude(\X^(X = correct; X = incomplete), Msgs, Errors),
    maplist(error_score, Errors, Scores),
    sumlist(Scores, Out).

% get_incomplete_lines(-IncompleteLines)
get_incomplete_lines(IL) :-
    get_lines(Lines),
    maplist(\X^(parse_line(X, [])), Lines, Msgs),
    zip(Lines, Msgs, Zipped),
    include(\X^(second(X, incomplete)), Zipped, IL).

% autocomplete(+Line, +Stack, -AutoCompleteScore)
autocomplete([], Stack, GenL) :-
    autocomplete_gen_(Stack, 0, GenL).
autocomplete([H|R], Stack, GenL) :-
    left_bracket(H),
    autocomplete(R, [H|Stack], GenL).
autocomplete([H|R], [S|RS], GenL) :-
    right_bracket(H),
    pair_brackets(S, H),
    autocomplete(R, RS, GenL).

autocomplete_gen_([], FinScore, FinScore).
autocomplete_gen_([S|RS], CurrScore, FinScore) :-
    pair_brackets(S, RB),
    autocomplete_score(RB, AS),
    Add #= CurrScore * 5 + AS,
    autocomplete_gen_(RS, Add, FinScore).

task_2(O) :-
    get_incomplete_lines(ILP),
    maplist(first, ILP, IL),
    maplist(\X^(autocomplete(X, [])), IL, Scores),
    length(Scores, Len),
    Middle #= (Len + 1) // 2,
    sort(0, @=<, Scores, SortedScores),
    nth1(Middle, SortedScores, O).
