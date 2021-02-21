-module(water).

-include("logger.hrl").
-include("user.hrl").

-define(RATIO, 10000).
-define(MAX_BROKERAGE, 500).

-export([start/0, draw/2, mixture/3, create_pool/4, status/1]).
-export([add_advance/2, modify_brokerage/2, modify_boundary/2]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    % Query = db:select([id], pool) ++ db:where([{id, "=", 1}]),
    Query = db:select([id], pool),
    {ok, Data} = db:query(Query),
    lists:foreach(fun(X) ->
        #{id := ID} = X,
        pool_sup:start_child([ID])
    end, Data),
    ok.

create_pool(Brokerage, Advance, AppKey, Pools) ->
    case ?MAX_BROKERAGE >= Brokerage of
        true ->
            case do_create_pool(Brokerage, Advance) of
                {ok, ID} ->
                    update_user(AppKey, ID, Pools),
                    {ok, #{id => ID}};
                Error ->
                    Error
            end;
        false ->
            {error, 1001}
    end.    

draw(ID, Odds) -> 
    Flag = pool_server:draw(ID, Odds),
    case Flag of
        fail ->
            {error, 1002};
        _ ->
            {ok, #{hit => Flag}}
    end.

mixture(ID, Bet, Odds) -> 
    Flag = pool_server:mixture(ID, Bet, Odds),
    case Flag of
        fail ->
            {error, 1002};
        _ ->
            {ok, #{hit => Flag}}
    end.

status(ID) -> 
    Data = pool_server:status(ID),
    case Data of
        fail ->
            {error, 1002};
        _ ->
            {ok, Data}
    end.

add_advance(ID, Val) ->
    Flag = pool_server:add_advance(ID, Val),
    case Flag of
        fail ->
            {error, 1};
        _ ->
            {ok, []}
    end.

modify_brokerage(ID, Val) ->
    Flag = pool_server:modify_brokerage(ID, Val),
    case Flag of
        fail ->
            {error, 1};
        _ ->
            {ok, []}
    end.

modify_boundary(ID, Val) ->
    Flag = pool_server:modify_boundary(ID, Val),
    case Flag of
        fail ->
            {error, 1};
        _ ->
            {ok, []}
    end.


%%%===================================================================
%%% Internal
%%%===================================================================

do_create_pool(BrokerageRatio, Advance) ->
    Pot = Advance*?RATIO,
    Query = db:insert(pool, [
        {brokerage, BrokerageRatio},
        {advance, Advance},
        {pot, Pot},
        {boundary, Pot}
    ]),
    case db:query(Query ++ "; SELECT LAST_INSERT_ID() as id;") of
        {ok, [#{id := ID}]} ->
            pool_sup:start_child([ID]),
            {ok, ID};
        _ ->
            {error, 1}
    end.

update_user(AppKey, ID, Pools) ->
    NewPools = lists:reverse([ID|lists:reverse(Pools)]),
    Query = db:update(user, [{pools, NewPools}], [{appkey, "=", AppKey}]),
    db:query(Query),
    User = ets:lookup(user, AppKey),
    NewUser = User#user{pools = NewPools},
    ets:insert(user, NewUser),
    ok.