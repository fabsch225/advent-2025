-module(part1).
-export([main/0, count_paths/1, parse_input/1]).

-define(START_NODE, "you").
-define(END_NODE, "out").
-define(INPUT_FILENAME, "input.txt").


main() ->
    case file:read_file(?INPUT_FILENAME) of
        {ok, BinaryInput} ->
            Input = unicode:characters_to_list(BinaryInput),
            Graph = parse_input(Input),
            Result = count_paths(Graph),
            io:format("~p~n", [Result]);
        {error, Reason} ->
            io:format("Error reading file ~s: ~p~n", [?INPUT_FILENAME, Reason]),
            halt(1)
    end.

parse_input(Input) ->
    Lines = string:split(Input, "\n", all),
    lists:foldl(
        fun(Line, Acc) ->
            case parse_line(Line) of
                {ok, Device, Outputs} ->
                    Acc#{Device => Outputs};
                {error, _} ->
                    Acc
            end
        end,
        #{},
        Lines
    ).

parse_line(Line) ->
    case string:split(string:trim(Line), ":", leading) of
        [NameStr, OutputsStr] ->
            Name = string:trim(NameStr),
            Outputs = [string:trim(O) || O <- string:split(string:trim(OutputsStr), " ", all), O /= ""],
            {ok, Name, Outputs};
        _ ->
            {error, "Invalid line format"}
    end.

count_paths(Graph) ->
    {_, Count} = count_paths_recursive(Graph, #{}, ?START_NODE),
    Count.

count_paths_recursive(_Graph, Memo, Device) when Device =:= ?END_NODE ->
    {maps:put(Device, 1, Memo), 1};

count_paths_recursive(Graph, Memo, Device) ->
    case maps:find(Device, Memo) of
        {ok, Count} ->
            {Memo, Count};
        error ->
            Outputs = maps:get(Device, Graph, []),
            
            {FinalMemo, TotalPaths} = lists:foldl(
                fun(OutputDevice, {CurrentMemo, Acc}) ->
                    {NewMemo, Paths} = count_paths_recursive(Graph, CurrentMemo, OutputDevice),
                    {NewMemo, Acc + Paths}
                end,
                {Memo, 0},
                Outputs
            ),

            {maps:put(Device, TotalPaths, FinalMemo), TotalPaths}
    end.