-module(pool_callback).

-include("logger.hrl").
-include("pool.hrl").

%% 万分比
-define(SPEED_RATE, 1000).
-define(SPEED_BIG, 8000).
-define(BIG_ODDS, 500000).
% -define(BIG_ODDS, 1600000). 
-define(ASCENT_SPEED_RATE, 2000).

%%%===================================================================
%%% API
%%%===================================================================

-export([draw/3]).
-export([pool_status/2]).
-export([add_advance/2, modify_brokerage/2, modify_boundary/2]).

draw(OddsRaw, Data, Flag) ->
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
    Odds = erlang:trunc(OddsRaw * Ratio), 
    {NewPot, NewSuction} = increase_pot(Pot, PotRatio, Suction, Ratio),
    
    {Result, NewWave, NewSegment, ReNewPot, NewBonus} =
    case Flag of
        ascent ->
            ascent(Odds, NewPot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus);
        fall ->
            fall(Odds, NewPot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus)
    end,
    NewData = Data#pool_data{
        pot = ReNewPot,
        wave = NewWave,
        segment = NewSegment,
        suction = NewSuction,
        bonus = NewBonus
    },
    State = wave:get_state(ReNewPot, NewWave),
    {State, NewData, Result}.

%%%===================================================================

pool_status(State, Data) ->
    #pool_data{
        ratio = Ratio,
        pot = Pot,
        suction = Suction,
        bonus = Bonus,
        brokerage_ratio = BrokerageRatio
    } = Data,
    #{
        state => State,
        pot => Pot div Ratio,
        wave => Data#pool_data.wave,
        segment => Data#pool_data.segment,
        suction => Suction div Ratio,
        bonus => Bonus div Ratio,
        brokerage => Suction * BrokerageRatio div Ratio / Ratio
    }.    

%%%===================================================================

add_advance(ValRaw, Data) ->
    #pool_data{
        ratio = Ratio,
        pot = Pot,
        advance = Advance,
        base_line = BaseLine,
        boundary = Boundary
    } = Data,    
    Val = ValRaw * Ratio * 10,
    NewPot = Pot + Val,
    NewAdvance = Val+Advance,
    {Wave, Segment} = fresh_wave(NewPot, BaseLine, Boundary),
    NewData = Data#pool_data{
        pot = NewPot,
        wave = Wave,
        segment = Segment,
        advance = NewAdvance
    },
    State = wave:get_state(NewPot, Wave),
    {State, NewData}.

modify_brokerage(Val, Data) ->
    NewData = Data#pool_data{
        brokerage_ratio = Val
    },    
    NewData.

modify_boundary(Val, Data) ->
    #pool_data{
        ratio = Ratio
    } = Data,
    NewData = Data#pool_data{        
        boundary = Val * Ratio * 10
    },    
    NewData.

% count_player() ->
%     Count = get(period_bet),


%%%===================================================================
%%% Internal
%%%===================================================================

ascent(Odds, Pot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus) ->
    case analyzing_ascent(Odds, Pot, Segment) of
        true ->
            case ascent_run(Odds, Pot, Ratio) of
                true ->
                    {NewPot, NewBonus} = decrease_pot(Pot, Odds, Bonus),
                    {true, Wave, Segment, NewPot, NewBonus};
                false ->
                    ascent_action(Pot, Segment, Wave, BaseLine, Boundary, Bonus)
            end;
        false ->
            ascent_action(Pot, Segment, Wave, BaseLine, Boundary, Bonus)
    end.

fall(Odds, Pot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus) ->
    case analyzing_fall(Odds, Pot, BaseLine, Segment) of
        true ->
            case fall_run(Odds, Ratio) of
                true ->
                    fall_action(Odds, Pot, Segment, Wave, BaseLine, Boundary, Bonus);
                false ->
                    {false, Wave, Segment, Pot, Bonus}
            end;
        win ->
            fall_action(Odds, Pot, Segment, Wave, BaseLine, Boundary, Bonus);
        false ->
            {NewWave, NewSegment} = fresh_wave(Pot, BaseLine, Boundary),
            {false, NewWave, NewSegment, Pot, Bonus}
    end.

%% ===================================================================

increase_pot(Pot, PotRatio, Suction, Ratio) ->
    NewPot = Pot + PotRatio,
    NewSuction = Suction + Ratio,
    {NewPot, NewSuction}.

decrease_pot(Pot, Odds, Bonus) ->
    NewPot = Pot - Odds,
    NewBonus = Bonus + Odds,
    {NewPot, NewBonus}.

%% ===================================================================

analyzing_ascent(Odds, Pot, Segment) ->
    {Bottom, _} = Segment,
    case Bottom >= Pot - Odds of
        true ->
            false;
        false ->
            true
    end.

analyzing_fall(Odds, Pot, BaseLine, Segment) ->
    case BaseLine >= Pot - Odds of
        true ->
            false;
        false ->
            {Top, _} = Segment,
            case Pot > Top of
                true ->
                    win;
                false ->
                    true
            end
    end.

%% ===================================================================

ascent_run(Odds, _Pot, Ratio) ->
    NewOdds = Odds + Odds*?ASCENT_SPEED_RATE div Ratio,
    run(NewOdds, Ratio).    

fall_run(Odds, Ratio) ->
    NewOdds = 
    case Odds > ?BIG_ODDS of
        true ->
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

fall_action(Odds, Pot, Segment, Wave, BaseLine, Boundary, Bonus) ->
    {NewPot, NewBonus} = decrease_pot(Pot, Odds, Bonus),
    {NewWave, NewSegment} = fall_segment(NewPot, Segment, Wave, BaseLine, Boundary),
    {true, NewWave, NewSegment, NewPot, NewBonus}.