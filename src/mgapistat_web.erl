%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for mgapistat.

-module(mgapistat_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    initialize(),
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE),
    cleanup().

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                handle_get(Req, Path, DocRoot);
            'POST' ->
                handle_post(Req, Path, DocRoot);
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

initialize() ->
    application:start(mysql),
    MySQLServer = os:getenv("MGAPISTAT_MYSQLSERVER"),
    MySQLUsername = os:getenv("MGAPISTAT_MYSQLUSERNAME"),
    MySQLPasswd = os:getenv("MGAPISTAT_MYSQLPASSWD"),
    {ok, _Pid} = mysql:start_link(mysql, MySQLServer, MySQLUsername, MySQLPasswd, "mgapi"),
    ok.

cleanup() ->
    application:stop(mysql),
    ok.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

handle_get(Req, Path, DocRoot) ->
    Req:serve_file(Path, DocRoot).

handle_post(Req, Path, _DocRoot) ->
    try
        "ajax/" ++ Command = Path,
        {Status, Response} = route_process(Req, Command),
        Encoder = mochijson2:encoder([{utf8, true}]),
        Req:respond({Status, [{"Content-Type", "application/json"}], Encoder(Response)})
    catch
        error:{badmatch, _} ->
            Report = ["web request failed",
                      {path, Path},
                      {type, "error"},
                      {what, "badmatch"},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:not_found();
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

route_process(Req, Command) ->
    process_flag(trap_exit,true),
    Params = Req:parse_post(),
    Pid = spawn_link(list_to_atom(Command), execute, [self(), Params]),
    receive
        {Pid, Status, Response} ->
            {Status, Response};
        Other ->
            io:format("Error in route_process: ~p~n", [Other]),
            Message = "Command(" ++ Command ++ ") not found.",
            {404, {struct, [{code, 404},
                            {message, list_to_binary(Message)}]}}
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
