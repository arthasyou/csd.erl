-module(usercc).

-export([create_user/0]).

create_user() ->    
    AppKey = short_uuid:uuid(),
    AppSecret = cry_hash:sha256(AppKey++time:get_localtime_string()++"SecRet"),
    Pools = [],
    Created = time:now(),
    Query = db:insert(user, [
        {appkey, AppKey},
        {appsecret, AppSecret},
        {pools, transform:term_to_string(Pools)},
        {created, Created}
    ]),
    db:query(Query),
    {AppKey, AppSecret}.