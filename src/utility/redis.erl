-module(redis).

-export([start/0, set/2, get/1]).
-export([lpush/2, rpush/2, lpop/1, rpop/1]).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    {ok, Data} = application:get_env(water, redis),
    {Host,Port,DataBase,Password,ReconnectSleep,TimeOut} = prase_data(Data),
    {ok, C} = eredis:start_link(Host,Port,DataBase,Password,ReconnectSleep,TimeOut),
    recorder:insert(redis, C).

set(Key, Value) ->
    C = get_pid(),
    R = eredis:q(C, ["SET", Key, Value]),
    reply(R).

get(Key) ->
    C = get_pid(),
    R = eredis:q(C, ["GET", Key]),
    reply(R).

lpush(List, Value) ->
    C = get_pid(),
    R = eredis:q(C, ["LPUSH", List, Value]),
    reply(R).

rpush(List, Value) ->
    C = get_pid(),
    R = eredis:q(C, ["RPUSH", List, Value]),
    reply(R).

lpop(List) ->
    C = get_pid(),
    R = eredis:q(C, ["LPOP", List]),
    reply(R).

rpop(List) ->
    C = get_pid(),
    R = eredis:q(C, ["RPOP", List]),
    reply(R).


%%%===================================================================
%%% Internal
%%%===================================================================

prase_data(Data) ->
    [
        {_, Host},
        {_, Port},
        {_, DataBase},
        {_, Password},
        {_, ReconnectSleep},
        {_, TimeOut}
    ] = Data,
    {Host,Port,DataBase,Password,ReconnectSleep,TimeOut}.

get_pid() ->
    recorder:lookup(redis).

reply(R) ->
    case R of
        {ok, D} ->
            D;
        _ ->
            error
    end.