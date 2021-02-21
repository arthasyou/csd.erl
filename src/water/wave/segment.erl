-module(segment).

%%%===================================================================
%%% API
%%%===================================================================
-export([create_segment/2]).

create_segment(Wave, Pot) ->    
    [H|_] = Wave,
    self() ! sync_db,
    {Pot,H}.



%%%===================================================================
%%% Internal
%%%===================================================================