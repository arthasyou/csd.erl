%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2020 09:19
%%%-------------------------------------------------------------------
-module(web).
-author("ysx").

%% ==================================================
%% API
%% ==================================================
-export([start/0]).

start() ->
    {ok, Port} = application:get_env(water, http_port),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[...]", toppage_h, []}
        ]}
    ]),
    % PrivDir = code:priv_dir(water),
	% {ok, _} = cowboy:start_tls(https, [
	% 	{port, Port},
	% 	{cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
	% 	{certfile, PrivDir ++ "/ssl/server.crt"},
	% 	{keyfile, PrivDir ++ "/ssl/server.key"}
	% ], #{env => #{dispatch => Dispatch}}).

    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }).


%% ==================================================
%% Internal
%% ==================================================
