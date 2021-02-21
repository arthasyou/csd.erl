%%%-------------------------------------------------------------------
%% @doc logic public API
%% @end
%%%-------------------------------------------------------------------

-module(water_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	Reply = water_sup:start_link(),
	web:start(),    
    water:start(),
	Reply.

stop(_State) ->
    ok.

%% internal functions
