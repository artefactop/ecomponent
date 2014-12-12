-define(NS_PROCESSOR, tab_ns_processor).
-define(WLIST_TABLE, mmwl).
-define(ECOMPONENT_DEFAULT_TIMEOUT, 5000).

-record(matching, {
    id :: string(),
    ns :: atom(),
    processor :: atom(),
    tries :: integer(),
    packet :: term()
}).

-record(params, {
    type :: undefined | string(),
    from :: ecomponent:jid(),
    to :: ecomponent:jid(),
    ns :: atom(),
    payload :: term(),
    iq :: term(),
    features = [] :: [binary()],
    info = [] :: proplists:proplists(),
    server :: atom()
}).

-record(response, {
    ns :: atom(),
    params :: #params{}
}).

-record(monitor, {
    id :: string(),
    counter = 0 :: integer(),
    timestamp = now() :: erlang:timestamp()
}).

-record(timem, {
    id :: string(),
    packet :: term(),
    timestamp :: integer(),
    node = node() :: atom()
}).

-record(message, {
    type ::  undefined | string(),
    from :: ecomponent:jid(),
    to :: ecomponent:jid(),
    xmlel :: term(),
    server :: atom()
}).

-record(presence, {
    type ::  undefined | string(),
    from :: ecomponent:jid(),
    to :: ecomponent:jid(),
    xmlel :: term(),
    server :: atom()
}).
