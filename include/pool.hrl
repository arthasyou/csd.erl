-ifndef(POOL).
-define(POOL, true).

-record(pool_data, {
    id,
    ratio,              % 比率 (100=百分比，10000=万分比)
    face_value,         % 面值
    brokerage_ratio,    % 佣金比率
    pot_ratio,          % 池底比率
    bullet,             % 底分
    pot,                % 当前池底
    base_line,          % 底线
    boundary,           % 边界线
    suction,            % 吸码量
    bonus,              % 总赢分
    jackpot,            % 彩金
    advance,            % 垫分
    wave,               % 波浪
    segment             % 分段
}).

-endif.