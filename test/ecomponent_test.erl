-module(ecomponent_test).

-compile(export_all).

-include("ecomponent_test.hrl").

setup_test_() ->
    {setup, 
        fun init_per_suite/0,
        fun end_per_suite/1,
        fun (Config) -> [
            access_list_get_test(Config), 
            access_list_set_test(Config),
            config_test(Config), 
            disco_muted_test(Config),
            ping_test(Config), 
            disco_test(Config), 
            disco_info_test(Config),
            forward_response_module_test(Config),
            forward_ns_in_set_test(Config), 
            save_id_expired_test(Config), 
            coutdown_test(Config),
            message_test(Config), 
            presence_test(Config), 
            sync_send_test(Config),
            multiconnection_test(Config),
            processor_iq_test(Config),
            processor_message_test(Config),
            processor_presence_test(Config),
            multiping_test(Config),
            forward_acl_ns_in_set_test(Config)
        ] end
    }.

init_per_suite() ->
    mnesia:start(),
    ?meck_lager(false),
    ?meck_syslog(false),
    ?meck_component(),
    ?meck_metrics(),
    ?run_exmpp(),
    ?meck_config([
        {syslog_name, "ecomponent" },
        {jid, "ecomponent.test" },
        {server, "localhost" },
        {port, 8899},
        {pass, "secret"},
        {whitelist, [] }, %% throttle whitelist
        {access_list_get, []},
        {access_list_set, [
            {'com.ecomponent.ns/ns1', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns2', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns3', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns4', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns5', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns6', [<<"bob.localhost">>]}
        ]},
        {max_per_period, 15},
        {period_seconds, 8},
        {processors, [
            {default, {mod, dummy}}
        ]},
        {message_processor, {mod, dummy}},
        {presence_processor, {mod, dummy}},
        {features, [<<"jabber:iq:last">>]},
        {mnesia_callback, []}
    ]). 

end_per_suite(_Config) ->
    mnesia:stop(),
    meck:unload(),
    ok.

init(config_test) ->
    ?meck_config([
        {syslog_name, "ecomponent" },
        {jid, "ecomponent.test" },
        {server, "localhost" },
        {port, 8899},
        {pass, "secret"},
        {whitelist, [] }, %% throttle whitelist
        {access_list_get, []},
        {access_list_set, [
            {'com.ecomponent.ns/ns1', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns2', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns3', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns4', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns5', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns6', [<<"bob.localhost">>]}
        ]},
        {max_per_period, 15},
        {period_seconds, 8},
        {processors, [
            {default, {mod, dummy}}
        ]},
        {message_processor, {mod, dummy}},
        {presence_processor, {mod, dummy}},
        {features, [<<"jabber:iq:last">>]},
        {mnesia_callback, [
            {dummy, tables, []}
        ]}
    ]);
init(_) ->
    Conf = [
        {syslog_name, "ecomponent" },
        {jid, "ecomponent.test" },
        {server, "localhost" },
        {port, 8899},
        {pass, "secret"},
        {whitelist, [] }, %% throttle whitelist
        {access_list_get, []},
        {access_list_set, [
            {'com.ecomponent.ns/ns1', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns2', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns3', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns4', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns5', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns6', [<<"bob.localhost">>]}
        ]},
        {max_per_period, 15},
        {period_seconds, 8},
        {processors, [
            {default, {mod, dummy}}
        ]},
        {message_processor, {mod, dummy}},
        {presence_processor, {mod, dummy}},
        {features, [<<"jabber:iq:last">>]},
        {mnesia_callback, []}
    ],
    ?meck_config(Conf),
    meck:new(dummy, [non_strict]),
    {ok, _} = ecomponent:start_link(),
    {ok, _} = ecomponent_acl:start_link(),
    {ok, _} = ecomponent_con_worker:start_link({default,default}, "ecomponent.test", Conf).

-define(finish(), begin
    meck:unload(dummy),
    ecomponent:stop(),
    ecomponent_acl:stop(),
    ?_assert(true)
end).

-record(dummy, {id,name,value}).

config_test(_Config) ->
    init(config_test),
    meck:new(dummy, [non_strict]),
    meck:expect(dummy, tables, 0, [{dummy, ram_copies, record_info(fields, dummy)}]), 
    {ok, State, _Timeout} = ecomponent:init([]), 
    meck:unload(dummy), 
    mnesia:table_info(dummy, all), 
    lager:info("~p~n", [State]),
    timer:sleep(250), 
    ?_assertMatch(#state{
        jid = "ecomponent.test",
        maxPerPeriod = 15,
        periodSeconds = 8,
        processors = [{default, {mod, dummy}}],
        message_processor = {mod, dummy},
        presence_processor = {mod, dummy},
        maxTries = 3,
        requestTimeout = 10,
        syslogFacility = local7,
        syslogName = "ecomponent"}, State).

disco_muted_test(_Config) ->
    ecomponent_func_test:run("disco_muted_test"),
    ?_assert(true).

ping_test(_Config) ->
    ecomponent_func_test:run("ping_test"),
    ?_assert(true).

message_test(_Config) ->
    ecomponent_func_test:run("message_test"),
    ?_assert(true).

presence_test(_Config) ->
    ecomponent_func_test:run("presence_test"),
    ?_assert(true).

disco_info_test(_Config) ->
    ecomponent_func_test:run("disco_info_test"),
    ?_assert(true).

disco_test(_Config) ->
    ecomponent_func_test:run("disco_test"),
    ?_assert(true).

forward_response_module_test(_Config) ->
    ecomponent_func_test:run("forward_response_module_test"),
    ?_assert(true).

forward_ns_in_set_test(_Config) ->
    ecomponent_func_test:run("forward_ns_in_set_test"),
    ?_assert(true).

save_id_expired_test(_Config) ->
    ecomponent_func_test:run("save_id_expired_test"),
    ?_assert(true).

sync_send_test(_Config) ->
    ecomponent_func_test:run("sync_send_test"),
    ?_assert(true).

coutdown_test(_Config) ->
    init(countdown_test),
    St = #state{requestTimeout=3},
    ?assertEqual(100, ecomponent:get_countdown(St)),
    State = ecomponent:reset_countdown(St),
    timer:sleep(1000),
    2000 = ecomponent:get_countdown(State),
    timer:sleep(1000),
    1000 = ecomponent:get_countdown(State),
    timer:sleep(1000),
    100 = ecomponent:get_countdown(State),
    ?finish().

access_list_get_test(_Config) ->
    init(access_list_get_test),
    Bob1 = {undefined, "bob1.localhost", undefined},
    true = ecomponent_acl:access_list_get('com.ecomponent.ns/ns1', Bob1),
    ?finish().

access_list_set_test(_Config) ->
    init(access_list_set_test),
    Bob = {undefined, "bob.localhost", undefined},
    Bob1 = {undefined, "bob1.localhost", undefined},
    ?assert(ecomponent_acl:access_list_set('com.ecomponent.ns/ns1', Bob)),
    ?assertNot(ecomponent_acl:access_list_set('com.ecomponent.ns/ns1', Bob1)),
    ?finish().

multiconnection_test(_Config) ->
    ecomponent_func_test:run("multiconnection_test"),
    ?_assert(true).

processor_iq_test(_Config) ->
    ecomponent_func_test:run("processor_iq_test"),
    ?_assert(true).

processor_message_test(_Config) ->
    ecomponent_func_test:run("processor_message_test"),
    ?_assert(true).

processor_presence_test(_Config) ->
    ecomponent_func_test:run("processor_presence_test"),
    ?_assert(true).

multiping_test(_Config) ->
    ecomponent_func_test:run("multiping_test"),
    ?_assert(true).

forward_acl_ns_in_set_test(_Config) ->
    ecomponent_func_test:run("forward_acl_ns_in_set_test"),
    ?_assert(true).
