-module(current).
-export([execute/2]).

execute(Pid, Params) ->
    ApiTypes = proplists:get_value("apiTypes", Params),
    ApiTypeList = re:split(ApiTypes, ","),

    Self = self(),
    lists:foreach(fun(ApiType) ->
			  spawn_link(fun() -> get_current_status(Self, binary_to_list(ApiType)) end)
		  end, ApiTypeList),
    ResultList = utils:receive_results(length(ApiTypeList), []),
    Sorted = lists:map(fun(A) ->
			       element(2, A)
		       end, lists:sort(fun(A, B) ->
					       element(1, A) < element(1, B)
				       end, ResultList)),

    Pid ! {self(), 200, {struct, [{result, Sorted}]}}.

get_current_status(Pid, ApiType) ->
    Result = get_current_status(ApiType),
    Pid ! {self(), ApiType, Result}.

get_current_status(ApiType) ->
    mysql:prepare(ps1, <<"SELECT AVG(latency) FROM histories WHERE api_type = ?">>),
    {data, Result1} = mysql:execute(mysql, ps1, [ApiType]),
    [[Average | _] | _] = mysql:get_result_rows(Result1),

    mysql:prepare(ps2, <<"SELECT SQRT(SUM(POW(latency - ?, 2)) / COUNT(*)) FROM histories WHERE api_type = ?">>),
    {data, Result2} = mysql:execute(mysql, ps2, [Average, ApiType]),
    [[StdDeviation | _] | _] = mysql:get_result_rows(Result2),

    mysql:prepare(ps3, <<"SELECT AVG(latency) FROM histories WHERE api_type = ? AND access_time BETWEEN CURRENT_TIMESTAMP - INTERVAL 35 MINUTE AND CURRENT_TIMESTAMP">>),
    {data, Result3} = mysql:execute(mysql, ps3, [ApiType]),
    [[RecentAverage | _] | _] = mysql:get_result_rows(Result3),

    Deviation = 10 * (RecentAverage - Average) / StdDeviation + 50,

    {struct,
     [
      {apiType, list_to_binary(ApiType)},
      {recentAverage, utils:round_value(RecentAverage)},
      {average, utils:round_value(Average)},
      {stdDeviation, utils:round_value(StdDeviation)},
      {deviation, utils:round_value(Deviation)}
     ]
    }.
