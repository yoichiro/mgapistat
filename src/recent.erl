-module(recent).
-export([execute/2]).

execute(Pid, Params) ->
    ApiTypes = proplists:get_value("apiTypes", Params),
    ApiTypeList = re:split(ApiTypes, ","),
    
    Map = lists:map(
	    fun(ApiType) ->
		    mysql:prepare(ps4, <<"SELECT DATE_FORMAT(access_time, \"%H:%i\"), latency FROM histories WHERE api_type = ? AND access_time BETWEEN CURRENT_TIMESTAMP - INTERVAL 65 MINUTE AND CURRENT_TIMESTAMP">>),
		    {data, Result} = mysql:execute(mysql, ps4, [ApiType]),
		    Rows = mysql:get_result_rows(Result),
		    {binary_to_list(ApiType), Rows}
	    end,
	    ApiTypeList),
    
    Response = lists:map(
                 fun({ApiType, Rows}) ->
                         {Labels, Values} = divide_label_value(Rows),
                         {list_to_atom(ApiType),
                          {struct, [
                                    {labels, Labels},
                                    {values, Values}
                                   ]}
                         }
                 end,
                 Map),

    Pid ! {self(),
           200,
           {struct, [{code, 200}, {result, {struct, Response}}]}
          }.

divide_label_value(List) ->
    divide_label_value(List, [], []).

divide_label_value([Head | List], Labels, Values) ->
    [Label, Value] = Head,
    divide_label_value(List, [Label | Labels], [Value | Values]);
divide_label_value([], Labels, Values) ->
    {lists:reverse(Labels), lists:reverse(Values)}.

