% nvim: set syntax=prolog

% AoC 2023 Day 5

:- use_module(library(clpfd)).
:- consult('../helper.pl').

% pre-parsed input
:- consult('input_05').

% tuple_to_range(+((Start, RangeLen), Mapping), -((Start, RangeEnd), Mapping))
tuple_to_range(((Mapping, Start, RangeLen)), ((Start, RangeEnd), Mapping)) :-
    RangeEnd #= Start + RangeLen - 1.

% mapping_to_ranges(+Mapping, -SortedRanges)
mapping_to_ranges(Mapping, SortedRanges) :-
    maplist(tuple_to_range, Mapping, Ranges),
    sort(Ranges, SortedRanges).

% seed_tuple_to_seed_range(+(Seed1, Seed2), -(Seed1, RangeEnd))
seed_tuple_to_seed_range((Seed1, Seed2), (Seed1, RangeEnd)) :-
    RangeEnd #= Seed1 + Seed2 - 1.

% seeds_to_see_ranges(+Seeds, -SeedRanges)
seeds_to_seed_ranges([], []).
seeds_to_seed_ranges([S1, S2|R], [SR|SRList]) :-
    seed_tuple_to_seed_range((S1, S2), SR),
    seeds_to_seed_ranges(R, SRList).

% sorted_ranges(-Ranges)
sorted_ranges(Ranges) :-
    maps(M),
    maplist(mapping_to_ranges, M, Ranges).


% single_seed_mapping(+Ranges, +Seed, -NewSeed)
single_seed_mapping([], Seed, Seed).
single_seed_mapping([((Start, End), Mapping)|_], Seed, NewSeed) :-
    Seed #>= Start,
    Seed #=< End,
    NewSeed #= Seed - Start + Mapping.
single_seed_mapping([_|Ranges], Seed, NewSeed) :-
    single_seed_mapping(Ranges, Seed, NewSeed).

% single_seeds_one_map(+Ranges, +Seeds, -NewSeeds)
single_seeds_one_map(Mapping, Seeds, NewSeeds) :-
    maplist(single_seed_mapping(Mapping), Seeds, NewSeeds).

task_1(MinSeed) :-
    sorted_ranges(Ranges),
    seeds(Seeds),
    foldl(single_seeds_one_map, Ranges, Seeds, FinalSeeds),
    min_list(FinalSeeds, MinSeed).


% seed_range_mapping((SStart, SEnd), [((RStart, _), _)|_], SStart, [(SStart, SEnd)]) :-
%     % The seed range is smaller then any ranges
%     SEnd #< RStart.
% seed_range_mapping((_, SEnd), [], NextS, [(NextS, SEnd)]). % Seed range bigger than all ranges
% seed_range_mapping((_, SEnd), [((RStart, _), _)|_], NextS, [(NextS, SEnd)]) :-
%     % The seed range end is smaller than one range in the list now, we can stop
%     SEnd #< RStart,
%     NextS #< SEnd.
% seed_range_mapping((_, SEnd), [((RStart, _), _)|_], _, []) :-
%     % The seed range end is smaller than one range in the list now, we can stop
%     SEnd #< RStart.
% seed_range_mapping((SStart, SEnd), [((_, REnd), _)|Rest], NextS, OutRanges) :-
%     % Has not reach the first range that may overlap, continue without edditing
%     REnd #< NextS,
%     seed_range_mapping((SStart, SEnd), Rest, NextS, OutRanges).
% seed_range_mapping((SStart, SEnd), [((RStart, REnd), Mapping)|Rest], NextS, OutRanges) :-
%     max_list([SStart, RStart], SubRS),
%     min_list([SEnd, REnd], SubRE),
%     SubRS #\= NextS,
%     N2S #= SubRS - RStart + Mapping,
%     N2E #= SubRE - RStart + Mapping,
%     N = [(NextS, SubRS), (N2S, N2E)],
%     NewNextS #= SubRE,
%     seed_range_mapping((SStart, SEnd), Rest, NewNextS, RestRanges),
%     append(N, RestRanges, OutRanges).
% seed_range_mapping((SStart, SEnd), [((RStart, REnd), Mapping)|Rest], _, OutRanges) :-
%     max_list([SStart, RStart], SubRS),
%     min_list([SEnd, REnd], SubRE),
%     N2S #= SubRS - RStart + Mapping,
%     N2E #= SubRE - RStart + Mapping,
%     N = [(N2S, N2E)],
%     NewNextS #= SubRE,
%     seed_range_mapping((SStart, SEnd), Rest, NewNextS, RestRanges),
%     append(N, RestRanges, OutRanges).

% seed_ranges_one_map(_, [], []).
% seed_ranges_one_map(Mapping, [(SStart, SEnd)|SR], NewSRs) :-
%     seed_range_mapping((SStart, SEnd), Mapping, SStart, NewSR),
%     seed_ranges_one_map(Mapping, SR, RestSRs),
%     append(NewSR, RestSRs, NewSRs).

% task_2(MinSeed) :-
%     sorted_ranges(Ranges),
%     seeds(Seeds),
%     seeds_to_seed_ranges(Seeds, SeedRanges),
%     foldl(seed_ranges_one_map, Ranges, SeedRanges, FinalSeedRanges),
%     sort(FinalSeedRanges, [(MinSeed, _)|_]).
