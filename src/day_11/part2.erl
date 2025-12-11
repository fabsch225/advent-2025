-module(part2).
-export([main/0, parse_input/1]).

-define(START_NODE, "svr").
-define(END_NODE, "out").
-define(NODE_A, "dac").
-define(NODE_B, "fft").
-define(INPUT_FILENAME, "input.txt").

-type device() :: string().
-type outputs() :: [device()].
-type graph() :: #{device() => outputs()}.
-type memo() :: #{device() => integer()}.

main() ->
    case file:read_file(?INPUT_FILENAME) of
        {ok, BinaryInput} ->
            Input = unicode:characters_to_list(BinaryInput),
            Graph = parse_input(Input),
            
            Result = solve_part2(Graph),
            io:format("~p~n", [Result]);
            
        {error, Reason} ->
            io:format("Error reading file ~s: ~p~n", [?INPUT_FILENAME, Reason]),
            halt(1)
    end.

-spec solve_part2(graph()) -> integer().
solve_part2(Graph) ->
    A = ?NODE_A,
    B = ?NODE_B,
    Start = ?START_NODE,
    End = ?END_NODE,

    Paths_S_to_A = count_paths_to(Graph, Start, A),
    Paths_A_to_B = count_paths_to(Graph, A, B),
    Paths_B_to_E = count_paths_to(Graph, B, End),
    
    Path_Sequence_1 = Paths_S_to_A * Paths_A_to_B * Paths_B_to_E,

    Paths_S_to_B = count_paths_to(Graph, Start, B),
    Paths_B_to_A = count_paths_to(Graph, B, A),
    Paths_A_to_E = count_paths_to(Graph, A, End),
    
    Path_Sequence_2 = Paths_S_to_B * Paths_B_to_A * Paths_A_to_E,

    Path_Sequence_1 + Path_Sequence_2.

-spec count_paths_to(graph(), device(), device()) -> integer().
count_paths_to(Graph, StartNode, EndNode) ->
    {_, Count} = count_paths_recursive(Graph, StartNode, EndNode, #{}),
    Count.

-spec count_paths_recursive(graph(), device(), device(), memo()) -> {memo(), integer()}.
count_paths_recursive(_Graph, Device, EndNode, Memo) when Device =:= EndNode ->
    {maps:put(Device, 1, Memo), 1};

count_paths_recursive(Graph, Device, EndNode, Memo) ->
    case maps:find(Device, Memo) of
        {ok, Count} ->
            {Memo, Count};
        error ->
            Outputs = maps:get(Device, Graph, []),
            
            {FinalMemo, TotalPaths} = lists:foldl(
                fun(OutputDevice, {CurrentMemo, Acc}) ->
                    {NewMemo, Paths} = count_paths_recursive(Graph, OutputDevice, EndNode, CurrentMemo),
                    {NewMemo, Acc + Paths}
                end,
                {Memo, 0},
                Outputs
            ),

            {maps:put(Device, TotalPaths, FinalMemo), TotalPaths}
    end.

-spec parse_input(string()) -> graph().
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

-spec parse_line(string()) -> {ok, device(), outputs()} | {error, string()}.
parse_line(Line) ->
    case string:split(string:trim(Line), ":", leading) of
        [NameStr, OutputsStr] ->
            Name = string:trim(NameStr),
            Outputs = [string:trim(O) || O <- string:split(string:trim(OutputsStr), " ", all), O /= ""],
            {ok, Name, Outputs};
        _ ->
            {error, "Invalid line format"}
    end.