-module(t).

-export([r/2, s/1]).

-define(ODDS, [20000, 30000,
40000,
50000,
60000,
80000,
90000,
100000,
150000,
200000,
250000,
300000,
500000,
600000,
1000000,
1200000,
1500000,
1800000,
2000000,
3000000,
6000000,
7000000,
8000000,
10000000]).

r(Times, Odds) ->    
    do(0, 0, Times, Odds).

do(W, L, 0, _Odds) ->
    {W, L};
do(W, L, T, Odds) ->
    % case pool_callback:F(200000, 10000) of
    % case pool_callback:run(200000, 10000) of
    case water:mixture(4, 1000, Odds) of
        {ok, #{hit := true}} ->
            do(W+1, L, T-1, Odds);
        {ok, #{hit := false}} ->
            do(W, L+1, T-1, Odds)
    end.

s(ID) ->
    pool_server:detail(ID).