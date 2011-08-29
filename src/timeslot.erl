-module(timeslot).
-export([execute/2]).

execute(Pid, Params) ->
    ApiTypes = proplists:get_value("apiTypes", Params),
    ApiTypeList = re:split(ApiTypes, ","),
    HistoryType = proplists:get_value("historyType", Params),
    load_and_response(Pid, ApiTypeList, HistoryType).
    
load_and_response(Pid, _, HistoryType)
  when HistoryType /= "avg", HistoryType /= "max" ->
    Pid ! {self(),
           400,
           {struct, []}
          };    
load_and_response(Pid, ApiTypeList, HistoryType) ->
    Self = self(),
    lists:foreach(fun(ApiType) ->
			  spawn_link(fun() ->
					     get_timeslot(Self,
							  binary_to_list(ApiType),
							  HistoryType)
				     end)
		  end, ApiTypeList),
    ResultList = utils:receive_results(length(ApiTypeList), []),
    Response = lists:map(
                 fun({ApiType, Rows}) ->
                         {Labels, Values} = utils:divide_label_value(Rows),
                         {list_to_atom(ApiType),
                          {struct, [
                                    {labels, Labels},
                                    {values, Values}
                                   ]}
                         }
                 end, ResultList),
    
    Pid ! {self(), 200,
	   {struct, [{result, {struct, Response}}]}}.

get_timeslot(Pid, ApiType, HistoryType) ->
    Result = get_timeslot(ApiType, HistoryType),
    Pid ! {self(), ApiType, Result}.

get_timeslot(ApiType, HistoryType) ->
    mysql:prepare(ts1, list_to_binary("SELECT HOUR(access_time), " ++ HistoryType ++ "(latency) FROM histories WHERE api_type = ? GROUP BY HOUR(access_time) ORDER BY HOUR(access_time)")),
    {data, Result} = mysql:execute(mysql, ts1, [ApiType]),
    mysql:get_result_rows(Result).

