-module(pool_init).

-include("logger.hrl").
-include("pool.hrl").

-export([init/1, sync_db/1]).

%%%===================================================================
%%% API
%%%===================================================================
init(ID) ->
    Qeury = db:select('pool') ++ db:where([{id, "=", ID}]),
    {ok, [Item]} = db:query(Qeury),
    Data = get_data(ID, Item),
    NewData= reside_wave(Data),
    State = get_state(NewData),
    {State, NewData}.

%%%===================================================================
%%% Internal
%%%===================================================================
get_data(ID, Item) ->
    #{        
        advance := Advance,
        base_line := BaseLine,
        bonus := Bonus,
        boundary := Boundary,
        brokerage := BrokerageRatio,
        bullet := Bullet,
        pot := Pot,
        jackpot := JackPot,
        ratio := Ratio,
        face_value := FaceValue,
        suction := Suction,
        wave := Wave,
        segment := Segment
    } = Item,
    #pool_data{
        id = ID,
        ratio = Ratio,
        face_value = FaceValue,
        brokerage_ratio = BrokerageRatio, 
        pot_ratio = Ratio - BrokerageRatio,       
        bullet = Bullet,         
        pot = Pot,
        base_line = BaseLine,      
        boundary = Boundary,       
        suction = Suction,        
        bonus = Bonus,         
        jackpot = JackPot,        
        advance = Advance,
        wave = transform:string_to_term(Wave),
        segment = transform:string_to_term(Segment)
    }.

get_state(Data) ->
    #pool_data{
        wave = Wave,
        pot = Pot
    } = Data,
    wave:get_state(Pot, Wave).

reside_wave(Data) ->
    #pool_data{
        wave = Wave,
        segment = Segment,
        pot = Pot,
        base_line = BaseLine,
        boundary = Boundary
    } = Data,
    NewWave =
    case Wave of
        undefined ->
            wave:create_wave(Pot, BaseLine, Boundary);
        [] ->
            wave:create_wave(Pot, BaseLine, Boundary);
        _ ->
            Wave
    end,  
    NewSegment = reside_segment(NewWave, Segment, Pot),
    Data#pool_data{
        wave = NewWave,
        segment = NewSegment
    }.

reside_segment(Wave, Segment, Pot) ->
    case Segment of
        undefined ->
            segment:create_segment(Wave, Pot);
        _ ->
            Segment
    end.

sync_db(Data) ->
    #pool_data{
        id = ID,
        pot = Pot,
        suction = Suction,        
        bonus = Bonus,         
        wave = Wave,
        segment = Segment,
        advance = Advance,
        brokerage_ratio = Brokerage,
        boundary = Boundary
    } = Data,
    Query = db:update(pool, [
        {pot, Pot},
        {suction, Suction},
        {bonus, Bonus},
        {advance, Advance},
        {brokerage, Brokerage},
        {boundary, Boundary},
        {wave, transform:term_to_string(Wave)},
        {segment, transform:term_to_string(Segment)}
    ]) ++ db:where([{id, "=", ID}]),
    db:query(Query),
    ok.
