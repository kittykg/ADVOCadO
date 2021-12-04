% nvim: set syntax=prolog

% AoC 2021 Day 4

:- consult('../helper.pl').
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).


%-----------------------------------%
%            IO parsing             %
%-----------------------------------%

% parse_board_string(+String, -Numbers)
parse_board_string(S, Nums) :-
    string(S),
    split_string(S, " ", "", NewS),
    exclude(\X^(X = ""), NewS, FS),
    maplist(atom_number, FS, Nums).

% get_board(-Boards)
get_board(Boards) :-
    open('input_04_board', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    exclude(\X^(X = ""), SL, FSL),
    get_board_(FSL, Boards).

get_board_([], []).
get_board_(SL, [Board|L]) :-
    split_at(5, SL, BoardL, Rest),
    maplist(parse_board_string, BoardL, Board),
    get_board_(Rest, L).

% get_nums(-Numbers)
get_nums(Nums) :-
    open('input_04_nums', read, Stream),
    read_string_file(Stream, [S]), !,
    split_string(S, ",", "", SL),
    maplist(atom_number, SL, Nums).

%-----------------------------------%
%            Bingo task             %
%-----------------------------------%

% bingoed(+Numbers, +Boards)
bingoed(Nums, Board) :-
    (any(Nums+\B^(subset(B, Nums)), Board) -> true;
        !, transpose(Board, BoardT),
        any(Nums+\B^(subset(B, Nums)), BoardT)
    ).

% bingo_loop(+AllNumbers, +HowManyToTake, +Boards, -Result)
bingo_loop(AllNums, Take, Boards, Out) :-
    take(Take, AllNums, Nums), !,
    (any(Nums+\B^(bingoed(Nums, B)), Boards) ->
        include(Nums+\B^(bingoed(Nums, B)), Boards, [BingoB]),
        last(Nums, LastN),
        flatten(BingoB, BingoBNums),
        exclude(Nums+\BN^(member(BN, Nums)), BingoBNums, UnmarkedN),
        sum_list(UnmarkedN, UNS),
        Out #= UNS * LastN;
        NewTake #= Take + 1,
        bingo_loop(AllNums, NewTake, Boards, Out)
    ).

task_1(Out) :-
    get_nums(AllNums), !,
    get_board(Boards), !,
    bingo_loop(AllNums, 1, Boards, Out).

% bingo_loop_2(+AllNumbers, +HowManyToTake, +Boards, -Result)
bingo_loop_2(AllNums, Take, Boards, Out) :-
    take(Take, AllNums, Nums), !,
    exclude(Nums+\B^(bingoed(Nums, B)), Boards, NotWinBoards), !,
    NewTake #= Take + 1,
    (length(NotWinBoards, 1) ->
        bingo_loop(AllNums, NewTake, NotWinBoards, Out);
        bingo_loop_2(AllNums, NewTake, NotWinBoards, Out)
    ).

task_2(Out) :-
    get_nums(AllNums), !,
    get_board(Boards), !,
    bingo_loop_2(AllNums, 1, Boards, Out).
