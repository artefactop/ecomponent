-define(NS_PROCESSOR, tab_ns_processor).

-record(matching, {id, ns, processor}).
-record(state, {xmppCom, jid, pass, server, port, whiteList, maxPerPeriod, periodSeconds, processors}).
-record(params, {type, from, to, ns, payload, iq}).
