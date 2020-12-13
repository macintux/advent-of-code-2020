-module(part2).
-compile([export_all]).

part2() ->
    part2("input.txt").

part2(Filename) ->
    Fh = myio:open_file(Filename),
    {{X, Y}, Waypoint} = p2_navigate(split(myio:next_line(Fh)), Fh, {0, 0}, {10, 1}),
    io:format("~w / ~w~n", [{X, Y}, Waypoint]),
    abs(X) + abs(Y).

split(eof) ->
    eof;
split([D|Rest]) ->
    {D, list_to_integer(Rest)}.

p2_navigate(eof, _Fh, Ship, Waypoint) ->
    {Ship, Waypoint};
p2_navigate({$R, Degrees}, Fh, Ship, Waypoint) ->
    p2_navigate(split(myio:next_line(Fh)), Fh,
                Ship, add(Ship, move_waypoint(-Degrees, relative(Waypoint, Ship))));
p2_navigate({$L, Degrees}, Fh, Ship, Waypoint) ->
    p2_navigate(split(myio:next_line(Fh)), Fh,
                Ship, add(Ship, move_waypoint(Degrees, relative(Waypoint, Ship))));
p2_navigate({$F, HowFar}, Fh, Ship, Waypoint) ->
    NewShip = mult(Ship, relative(Waypoint, Ship), HowFar),
    p2_navigate(split(myio:next_line(Fh)), Fh,
                NewShip, add(NewShip, relative(Waypoint, Ship)));
p2_navigate({Dir, HowFar}, Fh, Ship, Waypoint) ->
    p2_navigate(split(myio:next_line(Fh)), Fh,
                Ship, add(Waypoint, dir(Dir, HowFar))).

mult({X1, Y1}, {X2, Y2}, Mult) ->
    {X1 + (X2*Mult), Y1 + (Y2*Mult)}.

%% Waypoint should be first argument, ship second.
relative({X1, Y1}, {X2, Y2}) ->
    {X1-X2, Y1-Y2}.

r2d(Rad) ->
    Rad * 180 / math:pi().

d2r(Deg) ->
    Deg * math:pi() / 180.

to_polar({X, Y}) ->
    {polar, math:sqrt((X*X) + (Y*Y)), r2d(math:atan2(Y, X))}.

from_polar({polar, R, Phi}) ->
    {round(R*math:cos(d2r(Phi))),
     round(R*math:sin(d2r(Phi)))}.

move_waypoint(Degrees, {X, Y}) ->
    move_waypoint(Degrees, to_polar({X, Y}));
move_waypoint(Degrees, {polar, R, Phi}) ->
    from_polar({polar, R, Phi+Degrees}).


test1() ->
    {polar, R, Phi} = to_polar({3, 4}),
    abs(Phi - 53.13) < 0.15 andalso abs(5.0 - R) < 0.01.

test2() ->
    from_polar(to_polar({9, -15})) == {9, -15}.

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
