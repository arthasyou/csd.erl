-ifndef(USER).
-define(USER, true).

-record(user, {appkey, appsecret, pools = []}).

-endif.