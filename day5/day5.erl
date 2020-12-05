-module(day5).
-compile(export_all).


part2() ->
    Fh = myio:open_file("input.txt"),
    part2_highest(myio:next_line(Fh), Fh, 0, 0).

part1() ->
    Fh = myio:open_file("input.txt"),
    part1_highest(myio:next_line(Fh), Fh, 0).

part1_highest(eof, _Fh, Highest) ->
    Highest;
part1_highest(Line, Fh, Highest) ->
    part1_highest(myio:next_line(Fh), Fh,
                  max(p1_process_ticket(Line),
                      Highest)).

part2_highest(eof, _Fh, Highest, Discovered) ->
    {Highest, p2_find_gaps(1, Highest, Discovered, [])};
part2_highest(Line, Fh, Highest, Discovered) ->
    New = p1_process_ticket(Line),
    part2_highest(myio:next_line(Fh), Fh,
                  max(New, Highest),
                  1 bsl (New-1) bor Discovered).

p2_find_gaps(Max, Max, _All, Accum) ->
    lists:reverse(Accum);
p2_find_gaps(Cur, Max, All, Accum) when (1 bsl (Cur-1)) band All == (1 bsl (Cur-1)) ->
    p2_find_gaps(Cur+1, Max, All, Accum);
p2_find_gaps(Cur, Max, All, Accum) ->
    p2_find_gaps(Cur+1, Max, All, [Cur|Accum]).


p1_process_ticket(Ticket) ->
    Row = p1_calc_row(string:slice(Ticket, 0, 7)),
    Col = p1_calc_col(string:slice(Ticket, 7, 3)),
    io:format("~w, ~w~n", [Row, Col]),
    Row*8 + Col.

p1_calc_row(Row) ->
    p1_bsp(Row, {0, 127}, {$F, $B}).

p1_calc_col(Col) ->
    p1_bsp(Col, {0, 7}, {$L, $R}).

p1_bsp([Bot], {Low, _High}, {Bot, _Top}) ->
    Low;
p1_bsp([Top], {_Low, High}, {_Bot, Top}) ->
    High;
p1_bsp([Bot|T], Range, {Bot, Top}) ->
    p1_bsp(T, lower_half(Range), {Bot, Top});
p1_bsp([Top|T], Range, {Bot, Top}) ->
    p1_bsp(T, upper_half(Range), {Bot, Top}).

upper_half({Bot, Top}) ->
    {Bot + half(Top - Bot)+1, Top}.

lower_half({Bot, Top}) ->
    {Bot, Bot + half(Top - Bot)}.

half(Val) ->
    ((Val + 1) div 2) - 1.
