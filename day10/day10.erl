-module(day10).
-compile([export_all]).

part1() ->
    part1("input.txt").

part1(Filename) ->
    Ints = lists:sort(myio:all_integers(Filename)),
    {One, Three} = count_diffs(0, Ints, {0, 0}),
    One * Three.

count_diffs(_Prev, [], {One, Three}) ->
    {One, Three+1};
count_diffs(Prev, [H|T], {One, Three}) ->
    case H - Prev of
        1 ->
            count_diffs(H, T, {One+1, Three});
        3 ->
            count_diffs(H, T, {One, Three+1});
        _ ->
            count_diffs(H, T, {One, Three})
    end.



%% Part 2 is effectively unsolvable without a key insight: the list of
%% inputs can be segmented at any point where two input values are 3
%% apart, since 3 is the maximum gap between adapters.
%%
%% So we chunk our list based on those gaps and multiply the
%% permutation tallies for each list.
part2() ->
    part2("input.txt").

part2(Filename) ->
    Ints = lists:sort(myio:all_integers(Filename)),
    Device = lists:last(Ints) + 3,
    ChunkedLists = find_3_breaks(lists:append([[0], Ints, [Device]])),
    lists:foldl(fun(L, Tally) -> Tally * count_permutations(L) end,
                1, ChunkedLists).

find_3_breaks(Ints) ->
    find_3_breaks(Ints, [], []).

find_3_breaks([Last], LocalAccum, GlobalAccum) ->
    [lists:reverse([Last|LocalAccum])|GlobalAccum];
find_3_breaks([H1,H2|T], LocalAccum, GlobalAccum) when H2 - H1 == 3 ->
    find_3_breaks([H2|T], [], [lists:reverse([H1|LocalAccum])|GlobalAccum]);
find_3_breaks([H1,H2|T], LocalAccum, GlobalAccum) ->
    find_3_breaks([H2|T], [H1|LocalAccum], GlobalAccum).

count_permutations([Singleton]) ->
    1;
count_permutations([H|T]) ->
    count_permutations(H, T, lists:last(T)).

count_permutations(Prev, [Last], Last) when Last - Prev < 4->
    1;
count_permutations(Prev, [Last], Last) ->
    0;
count_permutations(Prev, [H|T], _Last) when H - Prev > 3 ->
    0;
count_permutations(Prev, [H|T], Last) ->
    count_permutations(H, T, Last) +
        count_permutations(Prev, T, Last).
