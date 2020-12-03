-module(day2).
-compile(export_all).

part1_re() ->
    {ok, MP} = re:compile("(\\d+)-(\\d+) (\\w): (\\w+)"),
    MP.

part1_eval_and_tally(true, Tally) ->
    Tally + 1;
part1_eval_and_tally(false, Tally) ->
    Tally.

part1_eval_passwd(Line, Re) ->
    {match, [_, Lower, Upper, Char, Password]} = re:run(Line, Re, [{capture, all, list}]),
    CharTally = lists:foldl(fun(C, Tally) when C == hd(Char) ->
                                    Tally+1;
                               (_C, Tally) ->
                                    Tally
                            end, 0, Password),
    part1_check_tally(CharTally, list_to_integer(Lower), list_to_integer(Upper)).

part1_check_tally(Tally, Lower, Upper) when Lower =< Tally, Upper >= Tally ->
    true;
part1_check_tally(_Tally, _Lower, _Upper) ->
    false.

part1() ->
    Re = part1_re(),
    myio:fold_lines(fun(X, Passed) -> part1_eval_and_tally(part1_eval_passwd(X, Re),
                                                           Passed) end, 0, "input.txt").

%% Part2 reuses several of part1's functions and all of its core flow.
part2() ->
    Re = part1_re(),
    myio:fold_lines(fun(X, Passed) -> part1_eval_and_tally(part2_eval_passwd(X, Re),
                                                           Passed) end, 0, "input.txt").

part2_eval_passwd(Line, Re) ->
    {match, [_, Idx1Str, Idx2Str, Char, Password]} = re:run(Line, Re, [{capture, all, list}]),
    part2_check_match(hd(Char), list_to_integer(Idx1Str), list_to_integer(Idx2Str), Password).

%% First clause turns out not to be necessary for any values in my
%% input, and it's not obvious if it *were* triggered that this would
%% be the correct behavior, since if one of the indexes is within the
%% length of the password and it matches, that probably should pass.
part2_check_match(_C, Idx1, Idx2, Password) when Idx1 > length(Password);
                                                 Idx2 > length(Password) ->
    false;
part2_check_match(C, Idx1, Idx2, Password) ->
    (lists:nth(Idx1, Password) == C) xor (lists:nth(Idx2, Password) == C).
