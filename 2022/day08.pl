% nvim: set syntax=prolog

% AoC 2022 Day 8

:- consult('../helper.pl').

% ---   Task logic   ---

% get_item(+Grid, +Y, +X, -Item)
get_item(Grid, Y, X, Item) :-
    nth0(X, Grid, Row),
    nth0(Y, Row, Item).

% get_column(+Grid, +Y, -Column)
get_column(Grid, Y, Column) :-
    length(Grid, Len),
    range(0, Len, Xs),
    maplist(get_item(Grid, Y), Xs, Column).

% get_tree_lists(+Grid, +Coord,
%       -CurrentTree, -LeftTrees, -RightTree, -TopTrees, -BottomTrees)
get_tree_lists(Grid, (X, Y), CurrTree, Left, Right, Top, Bot) :-
    nth0(X, Grid, FullRow),
    split_at(Y, FullRow, Left, [CurrTree|Right]), !,
    get_column(Grid, Y, FullColumn),
    split_at(X, FullColumn, Top, [CurrTree|Bot]), !.

% is_visible(+Grid, +Coord)
is_visible(Grid, (X, Y)) :-
    get_tree_lists(Grid, (X, Y), CurrTree, Left, _, _, _),
    is_visible_(CurrTree, Left), !.
is_visible(Grid, (X, Y)) :-
    get_tree_lists(Grid, (X, Y), CurrTree, _, Right, _, _),
    is_visible_(CurrTree, Right), !.
is_visible(Grid, (X, Y)) :-
    get_tree_lists(Grid, (X, Y), CurrTree, _, _, Top, _),
    is_visible_(CurrTree, Top), !.
is_visible(Grid, (X, Y)) :-
    get_tree_lists(Grid, (X, Y), CurrTree, _, _, _, Bot),
    is_visible_(CurrTree, Bot), !.

is_visible_(CurrTree, OtherTrees) :-
    max_list(OtherTrees, Max),
    CurrTree > Max.

% visibility_score(+Grid, +Coord, -Score)
visibility_score(Grid, (X, Y), Score) :-
    get_tree_lists(Grid, (X, Y), CurrTree, Left, Right, Top, Bot),
    reverse(Left, LeftR),
    visibility_score_(CurrTree, LeftR, LScore),
    visibility_score_(CurrTree, Right, RScore),
    reverse(Top, TopR),
    visibility_score_(CurrTree, TopR, TScore),
    visibility_score_(CurrTree, Bot, BScore),
    Score #= LScore * RScore * TScore * BScore.

visibility_score_(CurrTree, OtherTrees, Len) :-
    is_visible_(CurrTree, OtherTrees),
    length(OtherTrees, Len).
visibility_score_(CurrTree, OtherTrees, Score) :-
    visibility_score__(CurrTree, OtherTrees, 0, Score).

visibility_score__(CurrTree, [X|_], Score, NewScore) :-
    X #>= CurrTree,
    NewScore #= Score + 1.
visibility_score__(CurrTree, [_|L], Score, FinalScore) :-
    NewScore #= Score + 1,
    visibility_score__(CurrTree, L, NewScore, FinalScore).

% --- Input handling ---

get_input_grid(Grid) :-
    open('input_08', read, Stream),
    read_string_file(Stream, NL),
    maplist(string_to_digits, NL, Grid).

string_to_digits(N, L) :-
    string_codes(N, CL),
    maplist(number_code_, CL, L).

number_code_(C, N) :-
    number_codes(N, [C]).

% ---      Task      ---

% get_inner_indices(+Bound, -Coords)
get_inner_indices(Bound, Coords) :-
    range(1, Bound, Xs),
    maplist(get_inner_indices_(Bound), Xs, NestedCoords),
    flatten(NestedCoords, Coords).

get_inner_indices_(Bound, X, Coords) :-
    range(1, Bound, Ys),
    maplist(make_pair(X), Ys, Coords).

make_pair(X, Y, (X, Y)).

task_(G, I) :-
    get_input_grid(G),
    length(G, Len),
    Bound #= Len - 1,
    get_inner_indices(Bound, I).

task_1(O) :-
    task_(Grid, Indices),
    include(is_visible(Grid), Indices, Visibles),
    length(Visibles, O1),
    length(Grid, Width),
    O #= O1 + (Width - 1) * 4.

task_2(O) :-
    task_(Grid, Indices),
    maplist(visibility_score(Grid), Indices, Scores),
    max_list(Scores, O).
