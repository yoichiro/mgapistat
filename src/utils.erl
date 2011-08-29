-module(utils).
-export([round_value/1,
	divide_label_value/1,
	receive_results/2]).

round_value(Value) ->
    round(Value * 1000) / 1000.

divide_label_value(List) ->
    divide_label_value(List, [], []).

divide_label_value([Head | List], Labels, Values) ->
    [Label, Value] = Head,
    divide_label_value(List, [Label | Labels], [Value | Values]);
divide_label_value([], Labels, Values) ->
    {lists:reverse(Labels), lists:reverse(Values)}.

receive_results(Length, ResultList) when length(ResultList) == Length ->
    ResultList;
receive_results(Length, ResultList) ->
    receive
	{_Pid, ApiType, Result} ->
	    receive_results(Length, [{ApiType, Result} | ResultList])
    end.
