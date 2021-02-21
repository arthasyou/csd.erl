-module(web_error_message).

%% ==================================================
%% API
%% ==================================================
-export([get_msg/1]).

get_msg(1) ->    
    encode("系统错误");

get_msg(2) ->    
    encode("未知路径");

get_msg(3) ->
    encode("奖池ID错误");

get_msg(1001) ->    
    encode("佣金比率不可以超过计算比率的10%");

get_msg(1002) ->    
    encode("发送的ID不正确");

get_msg(_) ->    
    encode("未知错误").


%% ==================================================
%% Internal
%% ==================================================

encode(Msg) ->
    unicode:characters_to_binary(Msg).