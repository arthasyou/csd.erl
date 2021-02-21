%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Oct 2019 17:26
%%%-------------------------------------------------------------------
-module(web_callback).
-author("ysx").
-include("logger.hrl").

%% ==================================================
%% API
%% ==================================================
-export([create_pool/3, draw/2, mixture/2, pools/1, pool_detail/2]).

create_pool(DataIn, AppKey, Pools) ->
    #{
        <<"brokerage">> := Brokerage,
        <<"advance">> := Advance
    } = DataIn,    
    water:create_pool(Brokerage, Advance, AppKey, Pools).

draw(DataIn, Pools) ->
    #{
        <<"id">> := ID,
        <<"odds">> := Odds
    } = DataIn,
    case check_pool_id(ID, Pools) of
        true ->
            water:draw(ID, Odds);
        false ->
            {error, 3}
    end.

mixture(DataIn, Pools) ->
    #{
        <<"bet">> := Bet,
        <<"odds">> := Odds
    } = DataIn,
    ID =
    case Bet of
        B when B =< 30 ->
            lists:nth(1, Pools);
        B when B =< 100 ->
            lists:nth(2, Pools);
        B when B =< 400 ->
            lists:nth(3, Pools);
        _ ->
            lists:nth(4, Pools)
    end,
    water:mixture(ID, Bet, Odds).

pools(Pools) ->
    {ok, lists:sort(Pools)}.

pool_detail(DataIn, Pools) ->    
    #{
        <<"id">> := ID
    } = DataIn,
    case check_pool_id(ID, Pools) of
        true ->
            water:status(ID);
        false ->
            {error, 3}
    end.
    

%% ==================================================
%% Internal
%% ==================================================

check_pool_id(ID, Pools) ->
    lists:member(ID, Pools).
        