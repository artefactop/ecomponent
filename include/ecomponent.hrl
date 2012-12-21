-define(NS_PROCESSOR, tab_ns_processor).

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
    iq :: term()
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

-define(WLIST_TABLE, mmwl).

-record(timem, {
    id :: string(),
    packet :: term(),
    timestamp :: integer()
}).
