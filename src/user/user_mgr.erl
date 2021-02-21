-module(user_mgr).

-behaviour(gen_server).

-include("user.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	create_table(),
	loading(),
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_table() ->
	Option = [
		public, named_table, set,
		{keypos, #user.appkey},
		{write_concurrency, true}
	],
	ets:new(user, Option). 

loading() ->
	Query = db:select([appkey, appsecret, pools], user),
    {ok, Data} = db:query(Query),
    List = lists:map(fun(X) ->
        #{
			appkey := AppKey,
			appsecret := AppSecret,
			pools := Pools
		} = X,
		#user{
			appkey = erlang:binary_to_list(AppKey),
			appsecret = erlang:binary_to_list(AppSecret),
			pools = transform:string_to_term(Pools)
		}
    end, Data),
	ets:insert(user, List),
    ok.