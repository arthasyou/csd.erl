%%%-------------------------------------------------------------------
%% @doc main top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pool_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Args) ->	
    supervisor:start_child(?SERVER, Args).


init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 10},

    Server = #{
        id => pool_server,
        start => {pool_server, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [pool_server]
    },

    ChildSpecs = [Server],

    {ok, {SupFlags, ChildSpecs}}.
%% internal functions
