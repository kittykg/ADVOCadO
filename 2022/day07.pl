% nvim: set syntax=prolog

% AoC 2022 Day 7

:- consult('../helper.pl').

% insert_dir_trie(+Name, +Pwd, +Trie)
insert_dir_trie(Name, [], Trie) :-
    trie_new(T1),
    trie_insert(Trie, Name, T1).
insert_dir_trie(Name, [H|L], Trie) :-
    trie_lookup(Trie, H, SubTrie),
    insert_dir_trie(Name, L, SubTrie).

% insert_file_trie(+Name, +Size, +Pwd, +Trie)
insert_file_trie(Name, Size, [], Trie) :-
    trie_insert(Trie, Name, Size).
insert_file_trie(Name, Size, [H|L], Trie) :-
    trie_lookup(Trie, H, SubTrie),
    insert_file_trie(Name, Size, L, SubTrie).

% pharse_line(+Trie, +SplitedLineStr, +Pwd, -NewPwd)
pharse_line(Trie, ["dir", X], Pwd, Pwd) :-
    insert_dir_trie(X, Pwd, Trie).
pharse_line(_, ["$", "ls"], Pwd, Pwd).
pharse_line(_, ["$", "cd", ".."], Pwd, NewPwd) :-
    append(NewPwd, [_], Pwd).
pharse_line(_, ["$", "cd", Name], Pwd, NewPwd) :-
    append(Pwd, [Name], NewPwd).
pharse_line(Trie, [SizeStr, Name], Pwd, Pwd) :-
    number_string(Size, SizeStr),
    insert_file_trie(Name, Size, Pwd, Trie).

% split_string_(+String, -SplitedString)
split_string_(S, Sub) :-
    split_string(S, " ", "", Sub).

% get_input(-SplitedLines)
get_input(L) :-
    open('input_07', read, Stream),
    read_string_file(Stream, Lines),
    maplist(split_string_, Lines, L).

% is_leaf_folder(+Trie)
is_leaf_folder(Trie) :-
    get_trie_vals(Trie, Vals),
    length(Vals, N),
    exclude(is_trie, Vals, NonTrie),
    length(NonTrie, N).

% calculate_folder_size(+FolderSizes, +Trie, -NewFolderSizes)
calculate_folder_size(L, T, [Sum|L]) :-
    is_leaf_folder(T),
    get_trie_vals(T, Vals),
    sumlist(Vals, Sum).
calculate_folder_size(L, T, Out) :-
    get_trie_vals(T, Vals),
    exclude(is_trie, Vals, NonTrie),
    sumlist(NonTrie, FileSize),
    include(is_trie, Vals, Tries),
    maplist(calculate_folder_size([]), Tries, NestedL),
    flatten(NestedL, ListOfSums),
    maplist(head, NestedL, FSizes),
    sumlist(FSizes, FolderSize),
    Sum #= FileSize + FolderSize,
    append([Sum|ListOfSums], L, Out).

% get_all_folder_sizes(-AllFolderSizes)
get_all_folder_sizes(AllFolder) :-
    get_input(L),
    trie_new(T),
    foldl(pharse_line(T), L, [], _),
    calculate_folder_size([], T, AllFolder).

task_1(Out) :-
    get_all_folder_sizes(AllFolder),
    include(>=(100000), AllFolder, L),
    sumlist(L, Out).

task_2(Min) :-
    get_all_folder_sizes(AllFolder),
    max_list(AllFolder, Root),
    Del #= Root - (70000000 - 30000000),
    include(=<(Del), AllFolder, AllPossible),
    min_list(AllPossible, Min).
