%% @author Mochi Media <dev@mochimedia.com>
%% @copyright mgapistat Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the mgapistat application.

-module(mgapistat_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mgapistat.
start(_Type, _StartArgs) ->
    mgapistat_deps:ensure(),
    mgapistat_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mgapistat.
stop(_State) ->
    ok.
