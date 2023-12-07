% nvim: set syntax=prolog

% AoC 2023 Day 7

:- use_module(library(clpfd)).
:- consult('../helper.pl').

% pre-parsed input
:- consult('input_07').


% Five of a kind, where all five cards have the same label: AAAAA
five_of_a_kind(HandStr) :-
    string_chars(HandStr, Hand),
    list_to_set(Hand, [_]).

% Four of a kind, where four cards have the same label and one card has a
% different label: AA8AA
four_of_a_kind(HandStr) :-
    string_chars(HandStr, Hand),
    list_to_set(Hand, [C1, C2]),
    (count(C1, Hand, 4) ; count(C2, Hand, 4)).

% Full house, where three cards have the same label, and the remaining two cards
% share a different label: 23332
full_house(HandStr) :-
    string_chars(HandStr, Hand),
    list_to_set(Hand, [C1, C2]),
    (count(C1, Hand, 3) ; count(C2, Hand, 3)).

% Three of a kind, where three cards have the same label, and the remaining two
% cards are each different from any other card in the hand: TTT98
three_of_a_kind(HandStr) :-
    string_chars(HandStr, Hand),
    list_to_set(Hand, [C1, C2, C3]),
    (count(C1, Hand, 3) ; count(C2, Hand, 3) ; count(C3, Hand, 3)).

% Two pair, where two cards share one label, two other cards share a second
% label, and the remaining card has a third label: 23432
two_pair(HandStr) :-
    string_chars(HandStr, Hand),
    list_to_set(Hand, [C1, C2, C3]),
    count(C1, Hand, CC1),
    count(C2, Hand, CC2),
    count(C3, Hand, CC3),
    msort([CC1, CC2, CC3], [1, 2, 2]).

% One pair, where two cards share one label, and the other three cards have a
% different label from the pair and each other: A23A4
one_pair(HandStr) :-
    string_chars(HandStr, Hand),
    list_to_set(Hand, [C1, C2, C3, C4]),
    count(C1, Hand, CC1),
    count(C2, Hand, CC2),
    count(C3, Hand, CC3),
    count(C4, Hand, CC4),
    msort([CC1, CC2, CC3, CC4], [1, 1, 1, 2]).

% High card, where all cards' labels are distinct: 23456
high_card(HandStr) :-
    string_chars(HandStr, Hand),
    list_to_set(Hand, Set),
    length(Set, 5).


% hand_type(+HandStr, -Type)
hand_type(HandStr, 10) :-
    five_of_a_kind(HandStr).
hand_type(HandStr, 9) :-
    four_of_a_kind(HandStr).
hand_type(HandStr, 8) :-
    full_house(HandStr).
hand_type(HandStr, 7) :-
    three_of_a_kind(HandStr).
hand_type(HandStr, 6) :-
    two_pair(HandStr).
hand_type(HandStr, 5) :-
    one_pair(HandStr).
hand_type(HandStr, 4) :-
    high_card(HandStr).


% card_strength_cmp_(-Delta, +List, +Hand1, +Hand2)
card_strength_cmp_(=, _, [], []).
card_strength_cmp_(Delta, L, [C|T1], [C|T2]) :-
    card_strength_cmp_(Delta, L, T1, T2).
card_strength_cmp_(>, L, [C1|_], [C2|_]) :-
    nth1(I1, L, C1),
    nth1(I2, L, C2),
    I1 #> I2.
card_strength_cmp_(<, L, [C1|_], [C2|_]) :-
    nth1(I1, L, C1),
    nth1(I2, L, C2),
    I1 #< I2.

% card_strength_cmp_1(-Delta, +Hand1, +Hand2)
card_strength_cmp_1(Delta, Hand1, Hand2) :-
    L = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'],
    card_strength_cmp_(Delta, L, Hand1, Hand2).

% cmp_fn_1(-Delta, +Hand1, +Hand2)
cmp_fn_1(>, H1, H2) :-
    hand_type(H1, T1),
    hand_type(H2, T2),
    T1 > T2.
cmp_fn_1(<, H1, H2) :-
    hand_type(H1, T1),
    hand_type(H2, T2),
    T1 < T2.
cmp_fn_1(Delta, H1, H2) :-
    string_chars(H1, C1),
    string_chars(H2, C2),
    card_strength_cmp_1(Delta, C1, C2).
cmp_fn_1(=, _, _).

cmp_fn_1_(Delta, (H1, _), (H2, _)) :-
    cmp_fn_1(Delta, H1, H2).

% points(+(_, Bid), +(Rank, CurrPoints), -(NewRank, NewPoints))
points((_, Bid), (Rank, CurrPoints), (NewRank, NewPoints)) :-
    NewPoints #= Bid * Rank + CurrPoints,
    NewRank #= Rank + 1.

% sum_points(+SortedCards, -Points)
sum_points(SortedCards, Points) :-
    foldl(points, SortedCards, (1, 0), (_, Points)).

task_1(Points) :-
    input(Input),
    predsort(cmp_fn_1_, Input, Sorted),
    sum_points(Sorted, Points).


% replace_J(+Hand, +MaxCard, -NewHand)
replace_J([], _, []).
replace_J(['J'|R1], MaxCard, [MaxCard|R2]) :-
    replace_J(R1, MaxCard, R2).
replace_J([C|R1], MaxCard, [C|R2]) :-
    replace_J(R1, MaxCard, R2).

% count_occurrences(+Hand, +Char, -(Count, Char))
count_occurrences(Hand, Char, (Count, Char)) :- count(Char, Hand, Count).

% convert_card(+HandStr, -NewHandStr)
convert_card(HandStr, HandStr) :-
    string_chars(HandStr, Hand),
    \+ member('J', Hand), !.
convert_card(HandStr, HandStr) :-
    string_chars(HandStr, Hand),
    list_to_set(Hand, ['J']), !.
convert_card(HandStr, NewHandStr) :-
    string_chars(HandStr, Hand),
    list_to_set(Hand, Set),
    delete(Set, 'J', Set2),
    maplist(count_occurrences(Hand), Set2, Counts),
    sort(Counts, Sorted),
    reverse(Sorted, [(_, MaxCard)|_]),
    replace_J(Hand, MaxCard, NewHand),
    string_chars(NewHandStr, NewHand), !.

% card_strength_cmp_2(-Delta, +Hand1, +Hand2)
card_strength_cmp_2(Delta, Hand1, Hand2) :-
    L = ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A'],
    card_strength_cmp_(Delta, L, Hand1, Hand2).

% cmp_fn_2(-Delta, +Hand1, +Hand2)
cmp_fn_2(>, H1, H2) :-
    convert_card(H1, H1N),
    convert_card(H2, H2N),
    hand_type(H1N, T1),
    hand_type(H2N, T2),
    T1 > T2.
cmp_fn_2(<, H1, H2) :-
    convert_card(H1, H1N),
    convert_card(H2, H2N),
    hand_type(H1N, T1),
    hand_type(H2N, T2),
    T1 < T2.
cmp_fn_2(Delta, H1, H2) :-
    string_chars(H1, C1),
    string_chars(H2, C2),
    card_strength_cmp_2(Delta, C1, C2).
cmp_fn_2(=, _, _).

cmp_fn_2_(Delta, (H1, _), (H2, _)) :-
    cmp_fn_2(Delta, H1, H2).

task_2(Points) :-
    input(Input),
    predsort(cmp_fn_2_, Input, Sorted),
    sum_points(Sorted, Points).
