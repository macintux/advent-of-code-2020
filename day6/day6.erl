-module(day6).
-compile(export_all).

part1() ->
    Fh = myio:open_file("input.txt"),
    p1_count_groups(myio:next_line(Fh), Fh, sets:new(), 0).

p1_count_groups(eof, _Fh, Set, Sum) ->
    Sum + sets:size(Set);
p1_count_groups("", Fh, Set, Sum) ->
    p1_count_groups(myio:next_line(Fh), Fh, sets:new(),
                    Sum + sets:size(Set));
p1_count_groups(Chars, Fh, Set, Sum) ->
    p1_count_groups(myio:next_line(Fh), Fh,
                    lists:foldl(fun(C, S) -> sets:add_element(C, S) end,
                                Set, Chars),
                    Sum).



part2() ->
    Fh = myio:open_file("input.txt"),
    p2_count_groups(myio:next_line(Fh), Fh, undefined, 0).

p2_count_groups(eof, _Fh, Set, Sum) ->
    Sum + sets:size(Set);
p2_count_groups("", Fh, Set, Sum) ->
    p2_count_groups(myio:next_line(Fh), Fh, undefined,
                    Sum + sets:size(Set));
p2_count_groups(Chars, Fh, undefined, Sum) ->
    p2_count_groups(myio:next_line(Fh), Fh,
                    lists:foldl(fun(C, S) -> sets:add_element(C, S) end,
                                sets:new(), Chars),
                    Sum);
p2_count_groups(Chars, Fh, Set, Sum) ->
    p2_count_groups(myio:next_line(Fh), Fh,
                    sets:intersection(
                      lists:foldl(fun(C, S) -> sets:add_element(C, S) end,
                                  sets:new(), Chars), Set),
                    Sum).
