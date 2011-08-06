-module(access_api).
-export([main/0]).

main() ->
    initialize(),
    RefreshToken = get_refresh_token(),
    AccessToken = get_access_token(RefreshToken),
    send_request(AccessToken, 2, "/people/@me/@self"),
    send_request(AccessToken, 3, "/people/@me/@friends"),
    send_request(AccessToken, 4, "/updates/@me/@friends"),
    send_request(AccessToken, 5, "/voice/statuses/friends_timeline"),
    cleanup(),
    ok.

initialize() ->
    application:start(inets),
    inets:start(),
    ssl:start(),
    MySQLServer = os:getenv("MGAPISTAT_MYSQLSERVER"),
    MySQLUsername = os:getenv("MGAPISTAT_MYSQLUSERNAME"),
    MySQLPasswd = os:getenv("MGAPISTAT_MYSQLPASSWD"),
    {ok, _Pid} = mysql:start_link(mysql, MySQLServer, MySQLUsername, MySQLPasswd, "mgapi"),
    ok.

cleanup() ->
    ssl:stop(),
    inets:stop(),
    application:stop(mysql),
    ok.

get_refresh_token() ->
    mysql:prepare(ps1, <<"SELECT refresh_token FROM token">>),
    {data, Result} = mysql:execute(mysql, ps1, []),
    [[RefreshToken | _] | _] = mysql:get_result_rows(Result),
    binary_to_list(RefreshToken).

get_access_token(RefreshToken) ->
    ClientSecret = os:getenv("MGAPISTAT_CLIENTSECRET"),
    StartTime = current_time(),
    {ok, {_Info, _Headers, Body}} =
	httpc:request(
	  post, {
	    "https://secure.mixi-platform.com/2/token",
	    [],
	    "application/x-www-form-urlencoded",
	    "grant_type=refresh_token"
	    "&client_id=1593c37a0e1d567dc368"
	    "&client_secret=" ++ ClientSecret ++
	    "&refresh_token="
	    ++ RefreshToken
	   },
	  [], []),
    {struct, Token} = mochijson2:decode(Body),
    {_, AccessTokenBin} = lists:keyfind(<<"access_token">>, 1, Token),
    AccessToken = binary_to_list(AccessTokenBin),
    output_access_log(1, StartTime, current_time(), 1, 200, ""),
    AccessToken.

send_request(AccessToken, ApiType, PathInfo) ->
    StartTime = current_time(),
    {ok, {{_, Status, _}, Headers, Body}} =
	httpc:request(get, {
			"http://api.mixi-platform.com/2" ++ PathInfo,
			[{"Authorization", "OAuth " ++ AccessToken}]},
		      [], []),
    output_access_log(ApiType, StartTime, current_time(), 1, 200, "").

output_access_log(ApiType, StartTime, EndTime,
		  Succeed, HttpStatus, ErrorMessage) ->
    Latency = EndTime - StartTime,
    mysql:prepare(ps2, <<"INSERT INTO histories(api_type, access_time, latency, succeed, http_status, error_msg) VALUES (?, CURRENT_TIMESTAMP, ?, ?, ?, ?)">>),
    {updated, _Result} = mysql:execute(mysql, ps2, [ApiType, Latency, Succeed, HttpStatus, ErrorMessage]),
    ok.

current_time() ->
    {_, Sec, Micro} = now(),
    Sec * 1000 + Micro / 1000.
