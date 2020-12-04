-module(day3).
-compile(export_all).

part1_fold_chars($., Acc) ->
    [0|Acc];
part1_fold_chars($#, Acc) ->
    [1|Acc].

part1_fold_line(Line, Acc) ->
    [lists:foldr(fun part1_fold_chars/2, [], Line)|Acc].

%% Despite the fact that Erlang lists start with index 1, we need our
%% X value to start at 0 so wrapping at the end of the row works
%% correctly.
%%
%% We pass in X values between 0 and Width-1 (as managed by the
%% remainder operator), and then increment by 1 when we retrieve the
%% cell value.
part1_tally(Matrix, Cell, Adj, Tally) ->
    part1_tally(Matrix, Cell, Adj, length(lists:nth(1, Matrix)), Tally).

part1_tally(Matrix, {_X, Y}, _Adj, _Width, Tally) when Y > length(Matrix) ->
    Tally;
part1_tally(Matrix, {X, Y}, {XAdj, YAdj}=Adj, Width, Tally) ->
    TallyAdjustment = lists:nth(X+1, lists:nth(Y, Matrix)),
    part1_tally(Matrix, {(X+XAdj) rem Width, Y+YAdj}, Adj,
                Tally+TallyAdjustment).



part1() ->
    helper({3, 1}).

helper(Adj) ->
    part1_tally(lists:reverse(myio:fold_lines(fun part1_fold_line/2, [], "input.txt")), {0, 1}, Adj, 0).

part2() ->
    Adjustments = [
                   {1,1},
                   {3,1},
                   {5,1},
                   {7,1},
                   {1,2}
                  ],
    Results = lists:map(fun(Adj) -> helper(Adj) end, Adjustments),
    lists:foldl(fun(Val, Tally) -> Val * Tally end, 1, Results).
