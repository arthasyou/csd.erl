-module(web_reply).

-include("logger.hrl").

%% ==================================================
%% API
%% ==================================================
-export([handle/4]).

handle(Path, DataIn, AppKey, Pools) ->
    Reply = web_routes:routing(Path, DataIn, AppKey, Pools),
    encode(Reply).


%% ==================================================
%% Internal
%% ==================================================
encode(Reply) ->   
    % ?DEBUG("reply: ~p~n", [Reply]), 
    case Reply of
        {ok, Data} ->
            #{code => 0, data => Data};
        {error, Code} ->
            #{code => Code, reason => web_error_message:get_msg(Code)}
    end.