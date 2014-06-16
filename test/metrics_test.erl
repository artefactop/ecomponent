-module(metrics_test).
-compile(export_all).

-include("ecomponent_test.hrl").

init_test() ->
    application:start(folsom),
    ecomponent_metrics:init(),
    ?assertEqual([], folsom_metrics:get_metrics()).

dropped_test() ->
    ecomponent_metrics:notify_dropped_iq('get', 'jabber:iq:last'),
    ecomponent_metrics:notify_dropped_message('chat'),
    ecomponent_metrics:notify_dropped_presence('xa'),
    Metrics = folsom_metrics:get_metrics(),
    ?assert(lists:member('presence_dropped_xa', Metrics)),
    ?assert(lists:member('iq_dropped_get_jabber:iq:last', Metrics)),
    ?assert(lists:member('message_dropped_chat', Metrics)).

throughtput_test() ->
    ecomponent_metrics:notify_throughput_iq(in, get, 'jabber:iq:last'),
    ecomponent_metrics:notify_throughput_message(in, chat),
    ecomponent_metrics:notify_throughput_presence(in, xa),
    Metrics = folsom_metrics:get_metrics(),
    ?assert(lists:member('iq_throughput_in_get_jabber:iq:last', Metrics)),
    ?assert(lists:member('presence_throughput_in_xa', Metrics)),
    ?assert(lists:member('message_throughput_in_chat', Metrics)).
