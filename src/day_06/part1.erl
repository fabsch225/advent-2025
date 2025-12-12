-module(part1).
-export([main/0]).

is_blank_col(Col) ->
    lists:all(fun(C) -> C == $\s end, Col).

split_into_problems(Cols) ->
    split_into_problems(Cols, [], []).

split_into_problems([], Current, Acc) ->
    % Filter out groups that are all blank columns
    case lists:all(fun is_blank_col/1, Current) of
        true -> lists:reverse(Acc);
        false -> lists:reverse([lists:reverse(Current) | Acc])
    end;
split_into_problems([Col|Rest], Current, Acc) ->
    case is_blank_col(Col) of
        true ->
            case Current of
                [] -> split_into_problems(Rest, [], Acc);
                _ -> split_into_problems(Rest, [], [lists:reverse(Current) | Acc])
            end;
        false ->
            split_into_problems(Rest, [Col | Current], Acc)
    end.

trim(Str) ->
    % Trim leading and trailing spaces
    Trimmed1 = lists:dropwhile(fun(C) -> C == $\s end, Str),
    Trimmed2 = lists:reverse(
        lists:dropwhile(fun(C) -> C == $\s end, 
        lists:reverse(Trimmed1))
    ),
    Trimmed2.

parse_num(Str) ->
    Digits = [C || C <- Str, C >= $0, C =< $9],
    case Digits of
        [] -> nothing;
        _ -> {just, list_to_integer(Digits)}
    end.

parse_problem(Cols) ->
    Rows = transpose(Cols),
    % Trim each row (important!)
    TrimmedRows = [trim(Row) || Row <- Rows],
    OpRow = lists:last(TrimmedRows),
    
    % Find the operator
    Op = case lists:filter(fun(C) -> C == $+ orelse C == $* end, OpRow) of
            [] -> error("unknown operator");
            [OpChar|_] -> OpChar
         end,
    
    NumberRows = lists:droplast(TrimmedRows),
    % Parse numbers (using catMaybes equivalent)
    Numbers = [Num || {just, Num} <- [parse_num(Row) || Row <- NumberRows]],
    
    {Numbers, Op}.

eval_problem({Numbers, Op}) ->
    case Op of
        $+ -> lists:sum(Numbers);
        $* -> lists:foldl(fun(X, Acc) -> X * Acc end, 1, Numbers)
    end.

% Transpose a matrix
transpose([[]|_]) -> [];
transpose(Matrix) ->
    [lists:map(fun hd/1, Matrix) | transpose(lists:map(fun tl/1, Matrix))].

read_input() ->
    case file:read_file("input.txt") of
        {ok, Data} ->
            Lines = binary:split(Data, <<"\n">>, [global]),
            % Remove empty lines at the end
            NonEmpty = [Line || Line <- Lines, Line =/= <<>>],
            [binary_to_list(Line) || Line <- NonEmpty];
        {error, Reason} ->
            io:format("Error reading file: ~p~n", [Reason]),
            []
    end.

main() ->
    Lines = read_input(),
    
    % Find maximum width to pad properly
    MaxWidth = lists:max([length(Line) || Line <- Lines]),
    
    % Pad lines with spaces to make them uniform width
    Padded = [Line ++ lists:duplicate(MaxWidth - length(Line), $\s) || Line <- Lines],
    
    % Transpose to work with columns
    Cols = transpose(Padded),
    
    % Split into problems
    ProblemCols = split_into_problems(Cols),
    
    % Parse and evaluate problems
    Problems = [parse_problem(Problem) || Problem <- ProblemCols],
    Results = [eval_problem(Problem) || Problem <- Problems],
    
    io:format("~p~n", [lists:sum(Results)]),
    halt(0).