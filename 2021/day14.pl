% nvim: set syntax=prolog

% AoC 2021 Day 14

:- consult('../helper.pl').
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).

%-----------------------------------%
%            IO parsing             %
%-----------------------------------%

% parse_string_to_pair_insertions(+String, (-Pair, -Insertion))
parse_string_to_pair_insertion(S, ((A, B), V)) :-
    split_string(S, " -> ", "", [K, _, _, _, V]),
    string_chars(K, [ACh, BCh]),
    string_chars(A, [ACh]), string_chars(B, [BCh]).

% get_template(-PolymerTemplate)
get_template(Template) :-
    open('input_14_template', read, Stream),
    read_string_file(Stream, [Template]), !,
    close(Stream).

% get_pair_trie(-PairInsertionTrie)
get_pair_trie(Trie) :-
    open('input_14_pair', read, Stream),
    read_string_file(Stream, SL), !,
    close(Stream),
    maplist(parse_string_to_pair_insertion, SL, Pairs),
    trie_new(Trie),
    maplist(insert_pair_trie_(Trie), Pairs).

insert_pair_trie_(Trie, (K, V)) :- trie_insert(Trie, K, V).

% string_to_len_one_string_list(+String, -ListOfStrings)
string_to_len_one_strings_list(S, SL) :-
    string_chars(S, Chars),
    maplist(char_to_len_one_string, Chars, SL).

% char_to_len_one_string(+Char, -String)
char_to_len_one_string(C, S) :- string_chars(S, [C]).

%-----------------------------------%
%           Polymer Task            %
%-----------------------------------%

% progress_one_step(+PairInsertionTrie, +PairCountTrie,
%                   +CharCountTrie)
progress_one_step(PairTrie, PairCountTrie, CharCountTrie) :-
    get_trie_items(PairCountTrie, PairItems), !,
    maplist(
        progress_one_step_(PairTrie, PairCountTrie,
            CharCountTrie), PairItems
        ).

progress_one_step_(PairTrie, PairCountTrie,
                   CharCountTrie, ((A, B), Count)) :-
    trie_lookup(PairTrie, (A, B), I),
    NegCount #= 0 - Count,
    update_count_trie_plus(PairCountTrie, (A, B), NegCount),
    update_count_trie_plus(PairCountTrie, (A, I), Count),
    update_count_trie_plus(PairCountTrie, (I, B), Count),
    update_count_trie_plus(CharCountTrie, I, Count).

% update_count_trie_plus(+Trie, +Key, +Val)
update_count_trie_plus(Trie, Key, Val) :-
    (trie_gen(Trie, Key), ! ->
        trie_lookup(Trie, Key, CurrV),
        NewV #= CurrV + Val,
        trie_update(Trie, Key, NewV);
        trie_insert(Trie, Key, Val)
    ).

% progress(+PairInsertionTrie, +PairCountTrie,
%          +CharCountTrie, +StepLeft)
progress(_, _, _, 0).
progress(PairTrie, PairCountTrie, CharCountTrie, StepLeft) :-
    progress_one_step(PairTrie, PairCountTrie, CharCountTrie),
    NewStepLeft #= StepLeft - 1,
    progress(PairTrie, PairCountTrie, CharCountTrie, NewStepLeft).

% get_initial_tries(-PairCountTrie, -CharCountTrie)
get_initial_tries(PairCountTrie, CharCountTrie) :-
    get_template(P),
    string_to_len_one_strings_list(P, SL),
    get_counter_trie(SL, CharCountTrie),
    split_at(1, SL, _, SLR),
    uneven_zip(SL, SLR, AllPairs),
    get_counter_trie(AllPairs, PairCountTrie).

task(Steps, O) :-
    get_pair_trie(PairTrie),
    get_initial_tries(PairCountTrie, CharCountTrie), !,
    progress(PairTrie, PairCountTrie, CharCountTrie, Steps), !,
    get_trie_vals(CharCountTrie, AllCounts),
    max_list(AllCounts, Max), min_list(AllCounts, Min),
    O #= Max - Min.
