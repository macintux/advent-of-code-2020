-module(day12).
-compile([export_all]).

part1() ->
    part1("input.txt").

part1(Filename) ->
    Fh = myio:open_file(Filename),
    {Facing, {X, Y}} = p1_navigate(split(myio:next_line(Fh)), Fh, {90, {0, 0}}),
    abs(X) + abs(Y).

split(eof) ->
    eof;
split([D|Rest]) ->
    {D, list_to_integer(Rest)}.

p1_navigate(eof, _Fh, Where) ->
    Where;
p1_navigate({$R, Degrees}, Fh, {Facing, Coords}) ->
    p1_navigate(split(myio:next_line(Fh)), Fh,
                {Facing + Degrees, Coords});
p1_navigate({$L, Degrees}, Fh, {Facing, Coords}) ->
    p1_navigate(split(myio:next_line(Fh)), Fh,
                {Facing - Degrees, Coords});
p1_navigate({$F, HowFar}, Fh, {Facing, Coords}) ->
    p1_navigate(split(myio:next_line(Fh)), Fh,
                {Facing, add(Coords, xlate(Facing, HowFar))});
p1_navigate({Dir, HowFar}, Fh, {Facing, Coords}) ->
    p1_navigate(split(myio:next_line(Fh)), Fh,
                {Facing, add(Coords, dir(Dir, HowFar))}).

dir($N, Y) ->
    {0, Y};
dir($S, Y) ->
    {0, -Y};
dir($E, X) ->
    {X, 0};
dir($W, X) ->
    {-X, 0}.

add({X1, Y1}, {X2, Y2}) ->
    {X1+X2, Y1+Y2}.

to_360(Degrees) when Degrees < 0 ->
    360 + Degrees;
to_360(Degrees) ->
    Degrees.

xlate(Degrees, Units) ->
    normalized_xlate(to_360(Degrees rem 360), Units).


normalized_xlate(0, Units) ->
    {0, Units};
normalized_xlate(90, Units) ->
    {Units, 0};
normalized_xlate(180, Units) ->
    {0, -Units};
normalized_xlate(270, Units) ->
    {-Units, 0}.
