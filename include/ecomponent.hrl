-define(NS_PROCESSOR, tab_ns_processor).
-define(WLIST_TABLE, mmwl).

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
    features = [] :: [binary()]
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
    timestamp :: integer()
}).

-record(message, {
    type ::  undefined | string(),
    from :: ecomponent:jid(),
    xmlel :: term()
}).

-record(presence, {
    type ::  undefined | string(),
    from :: ecomponent:jid(),
    xmlel :: term()
}).
