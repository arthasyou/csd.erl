%%%-------------------------------------------------------------------
%% @doc logic top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(water_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    recorder:create(),
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 10},

    PoolSup = #{
        id => pool_sup,
        start => {pool_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [pool_sup]
    },

    PoolMgr = #{
        id => pool_mgr,
        start => {pool_mgr, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [pool_mgr]
    },

    UserMgr = #{
        id => user_mgr,
        start => {user_mgr, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [user_mgr]
    },

    QueueServer = #{
        id => db_queue_server,
        start => {db_queue_server, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [db_queue_server]
    },
    
    ChildSpecs = [PoolMgr, PoolSup, UserMgr, QueueServer],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
