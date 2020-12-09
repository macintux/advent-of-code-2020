-module(day9).
-compile(export_all).

part2(Filename, Target) ->
    Ints = myio:all_integers(Filename),
    p2_look_for_match(Ints, [], 0, Target).

p2_look_for_match(_List, Selection, Target, Target) ->
    lists:min(Selection) + lists:max(Selection);
p2_look_for_match([H|T], Selected, Sum, Target) when Sum + H > Target ->
    Len = length(Selected),
    NewSelection = lists:sublist(Selected, Len-1),
    Dropped = lists:last(Selected),
    p2_look_for_match([H|T], NewSelection, Sum-Dropped, Target);
p2_look_for_match([H|T], Selected, Sum, Target) ->
    p2_look_for_match(T, [H|Selected], Sum+H, Target).



part1() ->
    part1("input.txt", 25).

part1(Filename, PreambleCount) ->
    Ints = myio:all_integers(Filename),
    {Preamble, Rest} = lists:split(PreambleCount, Ints),
    look_for_failures(PreambleCount, lists:reverse(Preamble), Rest).

look_for_failures(Count, Preamble, [H|T]) ->
    case match_sum(Preamble, Count, H) of
        true ->
            look_for_failures(Count, [H|Preamble], T);
        false ->
            H
    end.

snippet(List, Count) ->
    lists:sublist(List, Count).

match_sum(List, Length, Sum) ->
    run_sums(snippet(List, Length), Sum).

run_sums([H|T], Sum) ->
    run_sums(H, T, T, Sum).

run_sums(_I, [], [], _Sum) ->
    false;
run_sums(_I, [], [H|T], Sum) ->
    run_sums(H, T, T, Sum);
run_sums(I, [H|T], List, Sum) ->
    case I+H of
        Sum ->
            true;
        _ ->
            run_sums(I, T, List, Sum)
    end.
