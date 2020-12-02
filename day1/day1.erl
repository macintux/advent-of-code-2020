-module(day1).
-compile(export_all).

run() ->
    Sorted = lists:sort(myio:all_integers("input.txt")),
    Reversed = lists:reverse(Sorted),
    {A, B, C} = part2(Sorted, Reversed, Reversed, Reversed, 2020),
    A*B*C.

test(Match) ->
    Vals = [1, 2, 3],
    Sorted = lists:sort(Vals),
    Reversed = lists:reverse(Sorted),
    {A, B, C} = part2(Sorted, Reversed, Reversed, Reversed, Match),
    A*B*C.

part1([L|_Ltail], [U|_Utail], _Upper, Match) when L + U == Match ->
    {L, U};
part1([L|Ltail], [U|_Utail], Upper, Match) when L + U < Match ->
    part1(Ltail, Upper, Upper, Match);
part1(Lower, [_U|Utail], Upper, Match) ->
    part1(Lower, Utail, Upper, Match).


%% All elements of the U2 list are too large for L+U1
part2(Lower, [_U1|Utail1], [], Upper, Match) ->
    part2(Lower, Utail1, Upper, Upper, Match);

%% Ran out of candidates from both upper lists
part2([_L|Ltail], [], Upper, Upper, Match) ->
    part2(Ltail, Upper, Upper, Upper, Match);

%% All elements of U1 list are too large for L+U2
part2(Lower, [], [_U2|Utail2], Upper, Match) ->
    part2(Lower, Upper, Utail2, Upper, Match);

%% Win!
part2([L|_Ltail], [U1|_Utail1], [U2|_Utail2], _Upper, Match) when L + U1 + U2 == Match ->
    {L, U1, U2};

%% We can stop checking head of U1
part2([L|_Ltail]=Lower, [U1|Utail1]=Working1, [U2|_Utail2], Upper, Match) when L + U1 + U2 < Match ->
    part2(Lower, Utail1, Upper, Upper, Match);

%% L+U1+U2 > Match
part2(Lower, Upper1, [_U2|Utail2], Upper, Match) ->
    part2(Lower, Upper1, Utail2, Upper, Match).
