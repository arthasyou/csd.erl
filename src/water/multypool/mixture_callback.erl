-module(mixture_callback).

-include("logger.hrl").
-include("pool.hrl").

-define(SPEED_RATE, 500). %% 万分比
-define(SPEED_BIG, 9000). %% 万分比
-define(BIG_ODDS, 300000). %% 万分比
-define(ASCENT_SPEED_RATE, 3000). %% 万分比

%%%===================================================================
%%% API
%%%===================================================================

-export([draw/4]).
-export([pool_status/1]).

draw(BetRaw, OddsRaw, Data, Flag) ->    
    #pool_data{
        ratio = Ratio,
        segment = Segment,
        pot = Pot,
        pot_ratio = PotRatio,
        base_line = BaseLine,
        boundary = Boundary,
        wave = Wave,
        suction = Suction,
        bonus = Bonus
    } = Data,
    Bet = erlang:trunc(BetRaw * Ratio),
    Win = erlang:trunc(Bet * OddsRaw),
    Odds = erlang:trunc(OddsRaw * Ratio),
    {NewPot, NewSuction} = increase_pot(Bet, Pot, PotRatio, Ratio, Suction),
    
    {Result, NewWave, NewSegment, ReNewPot, NewBonus} =
    case Flag of
        ascent ->
            ascent(Odds, Win, NewPot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus);
        fall ->
            fall(Bet, Odds, Win, NewPot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus)
    end,
    NewData = Data#pool_data{
        pot = ReNewPot,
        wave = NewWave,
        segment = NewSegment,
        suction = NewSuction,
        bonus = NewBonus
    },
    
    create_log(Result, BetRaw, OddsRaw*10),

    State = wave:get_state(ReNewPot, NewWave),
    {State, NewData, Result}.

%%%===================================================================

pool_status(Data) ->
    #pool_data{
        ratio = Ratio,
        pot = Pot,
        suction = Suction,
        bonus = Bonus,
        wave = Wave,
        segment = Segment,
        brokerage_ratio = BrokerageRatio
    } = Data,
    {H, _} = Segment,
    NewWave = lists:map(fun(X) ->
        X div (Ratio*10)
    end, [H|Wave]),

    #{
        pot => Pot / (Ratio*10),
        wave => NewWave,
        suction => Suction div (Ratio*10),
        bonus => Bonus / (Ratio*10),
        brokerage => Suction * BrokerageRatio div Ratio / Ratio / 10
    }.    

%%%===================================================================
%%% Internal
%%%===================================================================

ascent(Odds, Win, Pot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus) ->
    case analyzing_ascent(Win, Pot, Segment) of
        true ->
            case ascent_run(Odds, Pot, Ratio) of
                true ->
                    {NewPot, NewBonus} = decrease_pot(Pot, Win, Bonus),
                    {NewWave, NewSegment} = ascent_segment(NewPot, Segment, Wave, BaseLine, Boundary),
                    {true, NewWave, NewSegment, NewPot, NewBonus};
                false ->
                    ascent_action(Pot, Segment, Wave, BaseLine, Boundary, Bonus)
            end;
        false ->
            ascent_action(Pot, Segment, Wave, BaseLine, Boundary, Bonus)
    end.

fall(Bet, Odds, Win, Pot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus) ->
    case analyzing_fall(Win, Pot, BaseLine, Segment) of
        true ->
            case fall_run(Bet, Odds, Ratio) of
                true ->
                    fall_action(Win, Pot, Segment, Wave, BaseLine, Boundary, Bonus);
                false ->
                    {false, Wave, Segment, Pot, Bonus}
            end;
        win ->
            fall_action(Win, Pot, Segment, Wave, BaseLine, Boundary, Bonus);
        false ->
            {NewWave, NewSegment} = fresh_wave(Pot, BaseLine, Boundary),
            {false, NewWave, NewSegment, Pot, Bonus}
    end.

%% ===================================================================

increase_pot(Bet, Pot, PotRatio, Ratio, Suction) ->
    NewPot = Pot + erlang:trunc(Bet*PotRatio/Ratio),
    NewSuction = Suction + Bet,
    {NewPot, NewSuction}.

decrease_pot(Pot, Win, Bonus) ->
    NewPot = Pot - Win,
    NewBonus = Bonus + Win,
    {NewPot, NewBonus}.

%% ===================================================================

analyzing_ascent(Win, Pot, Segment) ->
    {Bottom, _} = Segment,
    case Bottom >= Pot - Win of
        true ->
            false;
        false ->
            true
    end.

analyzing_fall(_Win, Pot, _BaseLine, Segment) ->
    {Top, _} = Segment,
    case Pot > Top of
        true ->
            win;
        false ->
            true
    end.
    % true.
    % case BaseLine >= Pot - Win of
    %     true ->
    %         false;
    %     false ->
    %         true
    % end.

%% ===================================================================

ascent_run(Odds, _Pot, Ratio) ->
    NewOdds = Odds + Odds*?ASCENT_SPEED_RATE div Ratio,
    run(NewOdds, Ratio).    

fall_run(_Bet, Odds, Ratio) ->
    BigOdds = 200000,
    % case Bet of
    %     Temp when Temp =< 30 ->
    %         1000000;
    %     Temp when Temp =< 100 ->
    %         500000;
    %     _ ->
    %         300000
    % end,
    NewOdds = 
    case Odds >= BigOdds of
        true ->
            % 50000;
            Odds - Odds*?SPEED_BIG div Ratio;
        false ->
            Odds + Odds*?SPEED_RATE div Ratio
    end,
    run(NewOdds, Ratio).

run(Odds, Ratio) ->
    case Odds < Ratio of
        true ->
            true;
        false ->
            Rand = rand1:range(1,Odds),
            Rand =< Ratio
    end.

%% ===================================================================

ascent_segment(Pot, Segment, Wave, BaseLine, Boundary) ->
    {_, Destination} = Segment,
    case Pot >= Destination of
        true ->
            reside_wave_and_segment(Pot, Wave, BaseLine, Boundary);
        _ ->
            {Wave, Segment}
    end.

fall_segment(Pot, Segment, Wave, BaseLine, Boundary) ->
    {_, Destination} = Segment,
    case Pot =< Destination of
        true ->
            reside_wave_and_segment(Pot, Wave, BaseLine, Boundary);
        _ ->
            {Wave, Segment}
    end.

reside_wave_and_segment(Pot, Wave, BaseLine, Boundary) ->
    NewWave = reside_wave(Pot, Wave, BaseLine, Boundary),
    NewSegment = segment:create_segment(NewWave, Pot),    
    {NewWave, NewSegment}.

reside_wave(Pot, Wave, BaseLine, Boundary) ->
    [_|T] = Wave,
    case T of
        [] ->
            wave:create_wave(Pot, BaseLine, Boundary);
        _ ->
            T
    end.

fresh_wave(Pot, BaseLine, Boundary) ->
    Wave = wave:create_wave(Pot, BaseLine, Boundary),
    Segment = segment:create_segment(Wave, Pot),
    {Wave, Segment}.

%% =================================================================== 

ascent_action(Pot, Segment, Wave, BaseLine, Boundary, Bonus) ->
    {NewWave, NewSegment} = ascent_segment(Pot, Segment, Wave, BaseLine, Boundary),
    {false, NewWave, NewSegment, Pot, Bonus}.

fall_action(Win, Pot, Segment, Wave, BaseLine, Boundary, Bonus) ->
    {NewPot, NewBonus} = decrease_pot(Pot, Win, Bonus),
    {NewWave, NewSegment} = fall_segment(NewPot, Segment, Wave, BaseLine, Boundary),
    {true, NewWave, NewSegment, NewPot, NewBonus}.

%% =================================================================== 
create_log(Result, Bet, Odds) ->
    IsHit = 
    case Result of
        true ->
            1;
        false ->
            0
    end,
    Info = {bet_record, [
        {bet, Bet},
        {odds, Odds},
        {is_hit, IsHit},
        {created_at, time:now()}
    ]},
    Msg = {insert_data, Info},
    gen_server:cast(db_queue_server, Msg).