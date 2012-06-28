-define(NS_PROCESSOR, tab_ns_processor).

-record(matching, {id, ns, processor, tries, packet}).
-record(state, {xmppCom, jid, iqId, pass, server, port, whiteList, maxPerPeriod, periodSeconds, processors, maxTries, resendPeriod, requestTimeout}).
-record(params, {type, from, to, ns, payload, iq}).
-record(response, {ns, params}).
