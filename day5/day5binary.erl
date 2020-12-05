-module(day5binary).
-compile(export_all).

%% Wrote this after seeing discussion of the binary interpretation of
%% this day's problem and now I'm of course kicking myself.
%%
%% Taken from the problem description:
%%  BFFFBBFRRR: row 70, column 7, seat ID 567
%%
%% 2#1000110111 == 567


replace(String, V1, V2) ->
    lists:flatten(string:replace(String, V1, V2, all)).

%% B=>1, F=>0, R=>1, L=>0, then convert from a string of 1s and 0s to
%% the integer value.
interpret(String) ->
    list_to_integer(
      replace(
        replace(
          replace(
            replace(String, "R", "1"), "L", "0"),
          "B", "1"),
        "F", "0"), 2).

part1() ->
    Fh = myio:open_file("input.txt"),
    p1_highest(myio:next_line(Fh), Fh, 0).

p1_highest(eof, _Fh, Highest) ->
    Highest;
p1_highest(Seat, Fh, Highest) ->
    p1_highest(myio:next_line(Fh), Fh,
               max(interpret(Seat), Highest)).

%% This time, grab the highest, lowest, and sum of all values. Then
%% generate an "ideal" sum if all seats were full between lowest and
%% highest, and subtract; the remaining value is my seat.
part2() ->
    Fh = myio:open_file("input.txt"),
    {H, L, S} = p2_all(myio:next_line(Fh), Fh, 0, 1000, 0),
    lists:sum(lists:seq(L, H)) - S.

p2_all(eof, _Fh, Highest, Lowest, Sum) ->
    {Highest, Lowest, Sum};
p2_all(Seat, Fh, Highest, Lowest, Sum) ->
    Val = interpret(Seat),
    p2_all(myio:next_line(Fh), Fh,
           max(Val, Highest),
           min(Val, Lowest),
           Sum + Val).
