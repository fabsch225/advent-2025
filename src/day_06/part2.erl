-module(part2).
-export([main/0]).

is_blank_col(Col) ->
    lists:all(fun(C) -> C == $\s end, Col).

split_into_problems(Cols) ->
    split_into_problems(Cols, [], []).

split_into_problems([], Current, Acc) ->
    % Reverse to maintain original order and remove empty groups
    Filtered = [Group || Group <- lists:reverse([Current | Acc]), 
                not lists:all(fun is_blank_col/1, Group)],
    Filtered;
split_into_problems([Col|Rest], Current, Acc) ->
    case is_blank_col(Col) of
        true ->
            case Current of
                [] -> split_into_problems(Rest, [], Acc);
                _ -> split_into_problems(Rest, [], [Current | Acc])
            end;
        false ->
            split_into_problems(Rest, [Col | Current], Acc)
    end.

parse_problem(Cols) ->
    Rows = transpose(Cols),
    OpRow = lists:last(Rows),
    
    % Find the operator
    Op = case lists:filter(fun(C) -> C == $+ orelse C == $* end, OpRow) of
            [] -> error("unknown operator");
            [OpChar|_] -> OpChar
         end,
    
    NumberRows = lists:droplast(Rows),
    PerCol = transpose(NumberRows),
    
    % Extract numbers from each column
    Numbers = [list_to_integer(Digits) || 
               Col <- PerCol,
               Digits <- [lists:filter(fun(C) -> C >= $0 andalso C =< $9 end, Col)],
               Digits =/= []],
    
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

read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global]),
    % Remove empty lines and convert binaries to strings
    [binary_to_list(Line) || Line <- Lines, Line =/= <<>>].

main() ->
    Filename = "input.txt",
    Lines = read_input(Filename),
    
    % Calculate maximum width
    MaxW = lists:max([length(Line) || Line <- Lines]),
    
    % Pad lines with spaces to make them uniform width
    Padded = [Line ++ lists:duplicate(MaxW - length(Line), $\s) || Line <- Lines],
    
    % Transpose to work with columns
    Cols = transpose(Padded),
    
    % Split into problems
    Blocks = split_into_problems(Cols),
    
    % Parse and evaluate problems
    Problems = [parse_problem(Block) || Block <- Blocks],
    Results = [eval_problem(Problem) || Problem <- Problems],
    
    io:format("~p~n", [lists:sum(Results)]).