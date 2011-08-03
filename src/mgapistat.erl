%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc mgapistat.

-module(mgapistat).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the mgapistat server.
start() ->
    mgapistat_deps:ensure(),
    ensure_started(crypto),
    application:start(mgapistat).


%% @spec stop() -> ok
%% @doc Stop the mgapistat server.
stop() ->
    application:stop(mgapistat).
