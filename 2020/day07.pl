% nvim :set syntax=prolog

% AoC 2020 Day 7

:- consult('helper.pl').

% -------------------- Parser --------------------

take_first((Colour, _), L, [Colour|L]).
get_colours(K, HT, Colours) :- V = HT.get(K), foldl(take_first, V, [], Colours).

colours_string_to_term(S, Term) :-
    re_replace(" ", "_", S, Colour), term_string(Term, Colour).
colours_string_to_term(S1, S2, Term) :-
    string_concat(S1, "_", Temp), string_concat(Temp, S2, S),
    term_string(Term, S).

get_inside_bags(S, []) :- S = "no other bags".
get_inside_bags(S, L) :-
    split_string(S, " ", "", Substrings),
    get_inside_bags_(Substrings, L).

get_inside_bags_([], []).
get_inside_bags_([Num, C1, C2, _|Tail], [Pair|L]) :-
    string_to_number(Num, Count),
    colours_string_to_term(C1, C2, Term),
    Pair = (Term, Count),
    get_inside_bags_(Tail, L).

get_line_KV(S, K, V) :-
    re_matchsub("^(?<c>[a-zA-Z]+ [a-zA-Z]+) bags contain (?<r>.*).$", S, Sub, []),
    C = Sub.get(c), colours_string_to_term(C, K),
    R = Sub.get(r), get_inside_bags(R, V).

% ----------------------------------------

add_KV(S, HT, NewHT) :-
    get_line_KV(S, K, V),
    NewHT = HT.put([K=V]).
get_hash_table(L, HT) :- foldl(add_KV, L, ht{}, HT).

get_K(S, L, [K|L]) :- get_line_KV(S, K, _).
get_all_colours(L, Colours) :- foldl(get_K, L, [], Colours).

contain_sg_bag(K, HT) :-
    get_colours(K, HT, Colours),
    member(shiny_gold, Colours).
contain_sg_bag(K, HT) :-
    get_colours(K, HT, Colours),
    include_with_arg(contain_sg_bag, Colours, HT, ContainColours),
    \+ [] = ContainColours.

task_1(N) :-
    open('input_07', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream),
    get_all_colours(L, Colours),
    get_hash_table(L, HT),
    include_with_arg(contain_sg_bag, Colours, HT, Included),
    length(Included, N).

count_num_bags_inside(K, HT, 0) :- get_colours(K, HT, []).
count_num_bags_inside(K, HT, Total) :-
    V = HT.get(K),
    maplist(first, V, Colours), maplist(second, V, Counts),
    map_with_arg(count_num_bags_inside, Colours, HT, SubCounts),
    maplist(count_, Counts, SubCounts, Prod),
    sum_list(Prod, Total).

count_(Count, SubCount, Res) :- Res is Count * (SubCount + 1).

task_2(N) :-
    open('input_07', read, Stream),
    read_string_file(Stream, L), !,
    close(Stream),
    get_hash_table(L, HT),
    count_num_bags_inside(shiny_gold, HT, N).
