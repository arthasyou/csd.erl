-module(wave).

-include("logger.hrl").

-export([get_state/2, create_wave/3]).
-export([driving_wave/1, adjustment_wave/0, create_wave/1, span_wave/2]).

-define(GOLD_LESS, [0.3, 0.4, 0.4, 0.4, 0.5, 0.6]).
-define(GOLD_MORE, [1, 1.1, 1.2, 1.3]).
% -define(GOLD_LESS, [0.4, 0.5, 0.6, 0.7]).
% -define(GOLD_MORE, [1, 1.1, 1.2, 1.3, 1.4, 1.5]).

-define(GOLD_ADJST_MORE, [1, 1.1, 1.2, 1.3]).
-define(GOLD_ADJST_LESS, [0.6, 0.7, 0.8, 0.9]).

%%%===================================================================
%%% API
%%%===================================================================
get_state(Pot, Wave) ->
    [H|_] = Wave,
    case Pot > H of
        true ->
            fall;
        false ->
            ascent
    end.

create_wave(Pot, BaseLine, Boundary) ->
    case Pot =< 0 of
        true ->
            span_wave(Pot, Boundary);
        false ->
            Down = Pot - BaseLine,
            Up = Boundary - Pot,
            case Up > Down of
                true ->
                    span_wave(Pot, Boundary);
                false ->
                    span_wave(Pot, BaseLine)
            end
    end.
    % spec_wave().

%%%===================================================================
%%% Internal
%%%===================================================================

spec_wave() ->
    [90000000, 100000000].

%%%===================================================================

span_wave(From, To) ->
    Len = To - From,
    Wave = create_wave(Len),
    {W,_} = lists:mapfoldl(fun(X, Acc) ->
        {L1, _} = X,
        Point = trunc(Acc + L1),
        {Point, Point}
    end, From, Wave),
    % R = check_wave(W),
    W.
    % L = lists:mapfoldl(fun(_X, N) ->
    %     {N, N+1}
    % end, 1, R),
    % {L, R}.

check_wave(W) ->
    lists:map(fun(X) ->
        case X < 0 of
            true ->
                0;
            false ->
                X
        end
    end, W).

create_wave(Len) ->
    Ratios = driving_wave(5),
    Lens = ratio_to_len(Len, Ratios),
    create_wave_level(Lens, 3).

create_wave_level(Lens, 0) ->
    Lens;
create_wave_level(Lens, N) ->
    Lens2 = create_sub_wave(Lens, []),
    create_wave_level(Lens2, N-1).

create_sub_wave([], Rs) ->
    Rs;
create_sub_wave([H|T], Rs) ->
    {Len, Flag} = H,
    Ratios =
    case Flag of
        driving ->
            driving_wave(5);
        adjustment ->
            adjustment_wave()
    end,
    Wave = ratio_to_len(Len, Ratios),
    NewRs = Rs ++ Wave,
    create_sub_wave(T,  NewRs).

ratio_to_len(Len, Ratios) ->
    {List, _} =
    lists:mapfoldl(fun(X, Outset) ->
        {Ratio, Flag} = X,
        Destination = Len * Ratio,
        NewLen = Destination - Outset,
        {{NewLen, Flag}, Destination}
    end, 0, Ratios),
    List.

%%%===================================================================

driving_wave(N) ->
    List = lists:seq(1, N),
    Coefficients = span_driving_coefficient(List, 1, 1, []),
    Ratio = span_ratio(Coefficients),
    add_flag(Ratio).

span_driving_coefficient([], _, _, Results) ->
    lists:reverse(Results);
span_driving_coefficient([H|T], Base, Drive, Results) ->
    Ratio =
    case H of
        1 ->
            1;
        N when N rem 2 == 1 ->
            Base*rand1:range(?GOLD_MORE);
        _ ->
            -Drive*rand1:range(?GOLD_LESS)
    end,
    span_driving_coefficient(T, Base, Ratio, [Ratio|Results]).

adjustment_wave() ->
    List = lists:seq(1, 3),
    Coefficients = span_adjustment_coefficient(List, []),
    Ratio = span_ratio(Coefficients),
    add_flag(Ratio).

span_adjustment_coefficient([], Results) ->
    lists:reverse(Results);
span_adjustment_coefficient([H|T], Results) ->
    Ratio =
    case H of
        1 ->
            1;
        N when N rem 2 == 1 ->
            rand1:range(?GOLD_ADJST_MORE);
        _ ->
            -rand1:range(?GOLD_ADJST_LESS)
    end,
    span_adjustment_coefficient(T, [Ratio|Results]).

span_ratio(List) ->
    Base = 1/lists:sum(List),
    span_ratio(List, Base, 0, []).

span_ratio([], _, _, Results) ->
    lists:reverse(Results);
span_ratio([H|T], Base, Last, Results) ->
    Ratio = Base*H+Last,
    span_ratio(T, Base, Ratio, [Ratio|Results]).

add_flag(Ratios) ->
    add_flag(Ratios, 1, []).

add_flag([], _, R) ->
    lists:reverse(R);
add_flag([H|T], N, R) ->
    Flag =
    case N rem 2 of
        1 ->
            driving;
        0 ->
            adjustment
    end,
    add_flag(T, N+1, [{H,Flag}|R]).

%%%===================================================================
