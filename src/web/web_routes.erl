%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2019 15:07
%%%-------------------------------------------------------------------
-module(web_routes).
-author("ysx").

-include("logger.hrl").

%% ==================================================
%% API
%% ==================================================
-export([routing/4]).

routing(Path, DataIn, AppKey, Pools) ->
    % ?DEBUG("DataIn: ~p~n", [DataIn]),
    case format_path (Path) of
        "/create_pool" ->
            web_callback:create_pool(DataIn, AppKey, Pools);
        "/draw" ->
            web_callback:draw(DataIn, Pools);
        "/mixture" ->
            web_callback:mixture(DataIn, Pools);
        "/pools" ->
            web_callback:pools(Pools);
        "/pool_detail" ->
            web_callback:pool_detail(DataIn, Pools);
        _ ->
            other()
    end.


%% ==================================================
%% Internal
%% ==================================================

other() ->
    {ok, #{code => 2, reason => web_error_message:get_msg(2)}}.

format_path(Path) ->
	erlang:binary_to_list(Path).
