% nvim: set syntax=prolog

% AoC 2022 Day 6

:- consult('../helper.pl').

% get_input(-StringCodes)
get_input(S) :-
    open('input_06', read, Stream),
    read_string_file(Stream, [Str]),
    string_codes(Str, S).

% Method 1: Using include/3, but very slow

% is_marker(+StringCodes, +NumChar, +Start)
% is_marker(C, NumChar, I) :-
%     End #= I + NumChar,
%     sub_list(C, I, End, Sub), !,
%     list_to_set(Sub, Set),
%     length(Set, NumChar).

% % task_helper(+NumChar, -K)
% task_helper(NumChar, K) :-
%     get_input(S),
%     enum_list(S, Range),
%     include(is_marker(S, NumChar), Range, [M|_]),
%     K #= M + NumChar.

% task_1(K) :- task_helper(4, K).
% task_2(K) :- task_helper(14, K).

% Method 2: hard code the list pattern for unification, faster
% find_packet_marker(+List, +I, -K)
find_packet_marker([A,B,C,D|_], I, K) :-
    list_to_set([A,B,C,D], Set),
    length(Set, 4),
    K #= I + 4.
find_packet_marker([_,B,C,D|L], I, K) :-
    NewI #= I + 1,
    find_packet_marker([B,C,D|L], NewI, K).

% find_msg_marker(+List, +Index, -Out)
find_msg_marker([A,B,C,D,E,F,G,H,I,J,K,L,M,N|_], Index, Out) :-
    list_to_set([A,B,C,D,E,F,G,H,I,J,K,L,M,N], Set),
    length(Set, 14),
    Out #= Index + 14.
find_msg_marker([_,B,C,D,E,F,G,H,I,J,K,L,M,N|List], Index, Out) :-
    NewI #= Index + 1,
    find_msg_marker([B,C,D,E,F,G,H,I,J,K,L,M,N|List], NewI , Out).

task_1(K) :-
    get_input(S),
    find_packet_marker(S, 0, K).

task_2(K) :-
    get_input(S),
    find_msg_marker(S, 0, K).
