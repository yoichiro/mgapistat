-module(search).
-export([execute/2]).

execute(Pid, Params) ->
    Date = proplists:get_value("date", Params),
    ApiTypes = proplists:get_value("apiTypes", Params),
    ApiTypeList = re:split(ApiTypes, ","),
    HistoryType = proplists:get_value("historyType", Params),
    load_and_response(Pid, Date, ApiTypeList, HistoryType).

load_and_response(Pid, _, _, HistoryType)
  when HistoryType /= "avg", HistoryType /= "max" ->
    Pid ! {self(),
           400,
           {struct, []}
          };    
load_and_response(Pid, Date, ApiTypeList, HistoryType) ->
    Map = lists:map(
            fun(ApiType) ->
                    mysql:prepare(test, list_to_binary("SELECT HOUR(access_time), " ++ HistoryType ++ "(latency) FROM histories WHERE api_type = ? AND DATE(access_time) = ? GROUP BY HOUR(access_time)")),
                    {data, Result} = mysql:execute(mysql, test, [ApiType, list_to_binary(Date)]),
                    Rows = mysql:get_result_rows(Result),
                    {binary_to_list(ApiType), Rows}
            end,
            ApiTypeList),

    Response = lists:map(
                 fun({ApiType, Rows}) ->
                         {Labels, Values} = utils:divide_label_value(Rows),
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
           {struct, [{result, {struct, Response}}]}
          }.
