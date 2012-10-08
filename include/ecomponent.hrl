-define(NS_PROCESSOR, tab_ns_processor).

-record(matching, {
    id :: string(),
    ns :: string(),
    processor :: atom(),
    tries :: integer(),
    packet :: term()
}).

-record(params, {
    type,
    from,
    to,
    ns :: string(),
    payload,
    iq
}).

-record(response, {
    ns :: string(),
    params
}).

