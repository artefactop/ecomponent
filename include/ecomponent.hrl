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