-module(day7).
-compile([export_all]).

part1() ->
    Fh = myio:open_file("input.txt"),
    {_Outer, Inner} = process(myio:next_line(Fh),
                              Fh,
                              maps:new(), maps:new()),
    length(maps:keys(p1_chase(maps:find("shiny gold", Inner), Inner, maps:new()))).

p1_chase(error, _Map, Accum) ->
    Accum;
p1_chase({ok, ListOfBags}, Map, Accum) ->
    lists:foldl(fun(B, A) -> p1_chase(maps:find(B, Map), Map, maps:put(B, true, A)) end,
                Accum, ListOfBags).

process(eof, _Fh, OuterMap, InnerMap) ->
    {OuterMap, InnerMap};
process(Sentence, Fh, OuterMap, InnerMap) ->
    {match, [_, Outer, Inner]} = re:run(Sentence, "(.*) bags contain (.*)\.", [{capture, all, list}]),
    ListOfInner = parse_inner(Inner),
    process(myio:next_line(Fh), Fh,
            OuterMap#{Outer => ListOfInner},
            lists:foldl(fun({_C, I}, Map) -> maps:update_with(I, fun(O) -> [Outer|O] end, [Outer], Map) end,
                        InnerMap, ListOfInner)).



parse_inner("no other bags") ->
    [];
parse_inner(Bags) ->
    tuple_count(drop_empty(re:split(Bags, " bag.?,? ?", [{return, list}]))).

drop_empty(List) ->
    lists:filter(fun(E) -> E /= [] end, List).

tuple_count(List) ->
    lists:map(fun one_tuple/1, List).
one_tuple([C,$ |Rest]) ->
    {C-48, Rest}.

part2() ->
    Fh = myio:open_file("input.txt"),
    {Outer, _Inner} = process(myio:next_line(Fh),
                              Fh,
                              maps:new(), maps:new()),
    p2_chase(maps:find("shiny gold", Outer), Outer, 0).

p2_chase(error, _Map, Tally) ->
    Tally;
p2_chase({ok, ListOfBags}, Map, Tally) ->
    lists:foldl(fun({C, B}, A) -> A + C * p2_chase(maps:find(B, Map), Map, 1) end,
                Tally, ListOfBags).
