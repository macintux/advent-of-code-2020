-module(day4).
-compile(export_all).


part1_required_fields() ->
    sets:from_list(["byr", "iyr", "eyr", "hgt",
                    "hcl", "ecl", "pid", "cid"]).

part2_validation() ->
    #{
      "byr" => fun valid_byr/1,
      "iyr" => fun valid_iyr/1,
      "eyr" => fun valid_eyr/1,
      "hgt" => fun valid_hgt/1,
      "hcl" => fun valid_hcl/1,
      "ecl" => fun valid_ecl/1,
      "pid" => fun valid_pid/1,
      "cid" => fun(_M) -> 0 end
     }.

true_val(true) ->
    1;
true_val(_) ->
    0.

valid_byr(Year) ->
    Int = list_to_integer(Year),
    Int >= 1920 andalso Int =< 2002.

valid_iyr(Year) ->
    Int = list_to_integer(Year),
    Int >= 2010 andalso Int =< 2020.

valid_eyr(Year) ->
    Int = list_to_integer(Year),
    Int >= 2020 andalso Int =< 2030.

valid_hgt(Height) ->
    {Int, Units} = string:to_integer(Height),
    valid_hgt(Int, Units).

valid_hgt(Height, "cm") when Height >= 150 andalso Height =< 193 ->
    true;
valid_hgt(Height, "in") when Height >= 59 andalso Height =< 76 ->
    true;
valid_hgt(_, _) ->
    false.

valid_hcl([$#|Rest]) when length(Rest) == 6 ->
    string:take(Rest, "abcdef0123456789") == {Rest, []}.

valid_ecl("amb") ->
    true;
valid_ecl("blu") ->
    true;
valid_ecl("brn") ->
    true;
valid_ecl("gry") ->
    true;
valid_ecl("grn") ->
    true;
valid_ecl("hzl") ->
    true;
valid_ecl("oth") ->
    true;
valid_ecl(_) ->
    false.

valid_pid(PID) when length(PID) == 9 ->
    {_Int, []} = string:to_integer(PID),
    true.


part1_valid_tally(Map, Tally) ->
    case sets:to_list(sets:subtract(part1_required_fields(),
                                    sets:from_list(maps:keys(Map)))) of
        [] ->
            Tally+1;
        ["cid"] ->
            Tally+1;
        _ ->
            Tally
    end.

part2_valid_tally(Map, Tally) ->
    case sets:to_list(sets:subtract(part1_required_fields(),
                                    sets:from_list(maps:keys(Map)))) of
        [] ->
            part2_incr_if_valid(Map, Tally);
        ["cid"] ->
            part2_incr_if_valid(Map, Tally);
        _ ->
            Tally
    end.

valid_values(Map) ->
    Validations = part2_validation(),
    lists:foldl(fun(K, Ctr) -> Ctr + true_val(apply(maps:get(K, Validations),
                                                    [maps:get(K, Map)])) end,
                0, maps:keys(Map)).

part2_incr_if_valid(Map, Tally) ->
    try valid_values(Map) of
        7 ->
            io:format("Got one~n"),
            Tally + 1;
        Val ->
            io:format("~b out of 7~n", [Val]),
            Tally
    catch
        Err1:Err2 ->
            io:format("~s/~s~n", [Err1, Err2]),
            Tally
    end.



part1() ->
    Records = myio:file_to_maps("input.txt"),
    lists:foldl(fun part1_valid_tally/2, 0, Records).


part2() ->
    Records = myio:file_to_maps("input.txt"),
    lists:foldl(fun part2_valid_tally/2, 0, Records).
