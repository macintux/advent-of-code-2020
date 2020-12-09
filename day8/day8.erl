-module(day8).
-compile([export_all]).

instructions() ->
    #{
      "nop" => 0,
      "acc" => 1,
      "jmp" => 2
     }.

part1() ->
    Fh = myio:open_file("input.txt"),
    Instructions = load(myio:next_line(Fh), Fh, []),
    p1_trace(Instructions, 0, 0, []).

p1_trace(Instructions, Idx, Accumulator, Seen) ->
    case lists:member(Idx, Seen) of
        true ->
            Accumulator;
        _ ->
            {NextIdx, NextAccumulator} = interpret(Idx, lists:nth(Idx+1, Instructions), Accumulator),
            p1_trace(Instructions, NextIdx, NextAccumulator, [Idx|Seen])
    end.

interpret(Idx, {0, _Ign}, Acc) ->
    {Idx+1, Acc};
interpret(Idx, {1, Incr}, Acc) ->
    {Idx+1, Acc+Incr};
interpret(Idx, {2, Jump}, Acc) ->
    {Idx+Jump, Acc}.

load(eof, _Fh, Accum) ->
    lists:reverse(Accum);
load(Instruction, Fh, Accum) ->
    load(myio:next_line(Fh), Fh, [parse(Instruction)|Accum]).

parse([A,B,C,$ |Rest]) ->
    {maps:get([A,B,C], instructions()),
     list_to_integer(Rest)}.

%% Original version
%% parse([$n,$o,$p,$ |Rest]) ->
%%     {0, list_to_integer(Rest)};
%% parse([$a,$c,$c,$ |Rest]) ->
%%     {1, list_to_integer(Rest)};
%% parse([$j,$m,$p,$ |Rest]) ->
%%     {2, list_to_integer(Rest)}.

%% Not used currently
english({0, Int}) ->
    lists:flatten(io_lib:format("!! nop ~w", [Int]));
english({1, Int}) ->
    lists:flatten(io_lib:format("acc ~w", [Int]));
english({2, Int}) ->
    lists:flatten(io_lib:format("!! jmp ~w", [Int])).


%% `Ctr` and `Idx` appear similar but are orthogonal. `Ctr` increments
%% from 0 monotonically, `Idx` jumps around based on `jmp`
%% instructions.

%% `Idx` is an index into the list of instructions, starting with
%%    zero. Remember, however, that Erlang lists are 1-indexed, so we
%%    will grab the instruction at `Idx+1`.
%% `Accumulator` is the register we are incrementing based on `acc` instructions.
%% `Seen` is the instruction indexes we have encountered during this run, to detect an infinite loop.
%% `Ctr` represents the number of instructions we have seen during this run.
%% `LastAdjustment` is incremented each run so we know to skip that
%%    many operations before looking for a jmp/nop to swap. The value at
%%    run n+1 is the `Ctr` from the previous run where a swap occurred,
%%    plus 1.
%% `HaveAdjusted` indicates whether we've performed an operation swap this run
p2_dynamic(Instructions, Ctr, Idx, Accumulator, Seen, {LastAdjustment, _HaveAdjusted}=Adj) ->
    case lists:member(Idx, Seen) of
        true ->
            %% Halt before we enter an infinite loop
            io:format("."),
            p2_dynamic(Instructions, 0, 0, 0, [], {LastAdjustment+1, false});
        _ ->
            case Idx >= length(Instructions) of
                true ->
                    %% Success!
                    io:format("~n"),
                    Accumulator;
                false ->
                    Op = lists:nth(Idx+1, Instructions),
                    case {Adj, Op} of
                        {{LastAdjustment, false}, {0, Arg}} when LastAdjustment < Ctr ->
                            {NextIdx, NextAccumulator} = interpret(Idx, {2, Arg}, Accumulator),
                            %% Convert nop to jmp
                            p2_dynamic(Instructions, Ctr+1, NextIdx, NextAccumulator, [Idx|Seen], {Ctr, true});
                        {{LastAdjustment, false}, {2, Arg}} when LastAdjustment < Ctr ->
                            {NextIdx, NextAccumulator} = interpret(Idx, {0, Arg}, Accumulator),
                            %% Convert jmp to nop
                            p2_dynamic(Instructions, Ctr+1, NextIdx, NextAccumulator, [Idx|Seen], {Ctr, true});
                        _ ->
                            {NextIdx, NextAccumulator} = interpret(Idx, Op, Accumulator),
                            p2_dynamic(Instructions, Ctr+1, NextIdx, NextAccumulator, [Idx|Seen], Adj)
                    end
            end
    end.

part2() ->
    part2("input.txt").

part2(Filename) ->
    Fh = myio:open_file(Filename),
    Instructions = load(myio:next_line(Fh), Fh, []),
    p2_dynamic(Instructions, 0, 0, 0, [], {-1, false}).
