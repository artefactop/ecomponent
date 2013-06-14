-module(ecomponent_test).

-compile(export_all).

-include("../include/ecomponent_test.hrl").

setup_test_() ->
    {setup, 
        fun init_per_suite/0,
        fun end_per_suite/1,
        fun (Config) -> [
            disco_muted_test(Config),
            config_test(Config), 
            ping_test(Config), 
            disco_test(Config), 
            disco_info_test(Config),
            forward_response_module_test(Config),
            forward_ns_in_set_test(Config), 
            save_id_expired_test(Config), 
            coutdown_test(Config),
            message_test(Config), 
            presence_test(Config), 
            access_list_get_test(Config), 
            access_list_set_test(Config)
        ] end
    }.

init_per_suite() ->
    mnesia:start(),
    ?meck_lager(),
    ?meck_syslog(),
    ?meck_component(),
    ?meck_metrics(),
    ?run_exmpp(),
    ok.

end_per_suite(_Config) ->
    mnesia:stop(),
    meck:unload(),
    ok.

init(disco_info_test) ->
    ?meck_confetti([[{ecomponent, [
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
        {disco_info, true},
        {info, [
            {type, <<"jabber:last">>},
            {name, <<"Last Component">>}
        ]},
        {features, [<<"jabber:iq:last">>]}
    ]}]]),
    meck:new(dummy),
    {ok, _Pid} = ecomponent:start_link();
init(disco_muted_test) ->
    ?meck_confetti([[{ecomponent, [
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
        {disco_info, false}
    ]}]]),
    meck:new(dummy),
    {ok, _Pid} = ecomponent:start_link();
init(save_id_expired_test) ->
    ?meck_confetti([[{ecomponent, [
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
        {request_timeout, 2}
    ]}]]),
    meck:new(dummy),
    {ok, _Pid} = ecomponent:start_link();
init(_) ->
    ?meck_confetti([[{ecomponent, [
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
        {features, [<<"jabber:iq:last">>]}
    ]}]]),
    meck:new(dummy),
    {ok, _Pid} = ecomponent:start_link().

finish() ->
    ok = ecomponent:stop(),
    meck:unload(confetti),
    meck:unload(dummy),
    ?_assert(true).

config_test(_Config) ->
    init(config_test),
    {ok, State} = ecomponent:init([]),
    lager:info("~p~n", [State]),
    Pid = self(),
    finish(),
    ?_assertMatch({state, 
        Pid, "ecomponent.test", "secret",
        "localhost", 8899, [], 15, 8, [{default, {mod, dummy}}],
        {mod, dummy}, {mod, dummy},
        3, 100, 10, [
            {'com.ecomponent.ns/ns1', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns2', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns3', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns4', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns5', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns6', [<<"bob.localhost">>]}
        ], [], local7, "ecomponent", _Timestamp, _Features, _Info, _DiscoInfo}, State).

message_test(_Config) ->
    init(message_test),
    Packet = #received_packet{
        packet_type=message, type_attr="chat", raw_packet=
            ?Parse(<<"
                <message xmlns='jabber:client' 
                         type='chat' 
                         to='alice.localhost'
                         id='test_bot'>
                    <body>ping</body>
                </message>
            ">>),
        from={"bob","localhost",undefined}
    },
    Pid = self(),
    meck:expect(dummy, process_message, fun(Message) ->
        Pid ! Message
    end),
    ecomponent ! Packet,
    ?try_catch(#message{type="chat", xmlel=Xmlel}, 1000),
    finish().

presence_test(_Config) ->
    init(presence_test),
    Packet = #received_packet{
        packet_type=presence, type_attr=undefined, raw_packet=
            ?Parse(<<"
                <presence xmlns='jabber:client' 
                          to='alice.localhost' 
                          id='test_bot'/>
            ">>),
        from={"bob","localhost",undefined}
    },
    Pid = self(),
    meck:expect(dummy, process_presence, fun(Presence) ->
        Pid ! Presence
    end),
    ecomponent ! Packet,
    ?try_catch(#presence{xmlel=Xmlel}, 1000),
    finish().

disco_muted_test(_Config) ->
    init(disco_muted_test),
    DiscoPacket = #received_packet{
        packet_type=iq, type_attr="get", raw_packet=
            ?Parse(<<"
                <iq xmlns='jabber:client'
                    type='get'
                    to='ecomponent.test'
                    id='test_bot'>
                    <query xmlns='http://jabber.org/protocol/disco#info'/>
                </iq>
            ">>),
        from={"bob","localhost",undefined}
    },
    Packet = #received_packet{
        packet_type=iq, type_attr="get", raw_packet=
            ?Parse(<<"
                <iq xmlns='jabber:client'
                    type='get'
                    to='alice.localhost'
                    id='test_bot'>
                    <ping xmlns='urn:xmpp:ping'/>
                </iq>
            ">>),
        from={"bob","localhost",undefined}
    },
    Pid = self(),
    meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
        Pid ! P
    end),
    ecomponent ! DiscoPacket,
    ecomponent ! Packet,
    Reply = ?CleanXML(<<"
        <iq xmlns='jabber:client'
            type='result'
            id='test_bot'
            from='alice.localhost'/>
    ">>),
    ?try_catch_xml(Reply, 1000),
    finish().

ping_test(_Config) ->
    init(ping_test),
    Packet = #received_packet{
        packet_type=iq, type_attr="get", raw_packet=
            ?Parse(<<"
                <iq xmlns='jabber:client'
                    type='get'
                    to='alice.localhost'
                    id='test_bot'>
                    <ping xmlns='urn:xmpp:ping'/>
                </iq>
            ">>),
        from={"bob","localhost",undefined}
    },
    Pid = self(),
    meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
        Pid ! P
    end),
    ecomponent ! Packet,
    Reply = ?CleanXML(<<"
        <iq xmlns='jabber:client'
            type='result'
            id='test_bot'
            from='alice.localhost'/>
    ">>),
    ?try_catch_xml(Reply, 1000),
    finish().

disco_info_test(_Config) ->
    init(disco_info_test),
    Packet = #received_packet{
        packet_type=iq, type_attr="get", raw_packet=
            ?Parse(<<"
                <iq xmlns='jabber:client'
                    type='get'
                    to='alice.localhost'
                    id='test_bot'>
                    <query xmlns='http://jabber.org/protocol/disco#info'/>
                </iq>
            ">>),
        from={"bob","localhost",undefined}
    },
    Pid = self(),
    meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
        Pid ! P
    end),
    ecomponent ! Packet,
    Reply = ?CleanXML(<<"
        <iq xmlns='jabber:client'
            type='result'
            id='test_bot'
            from='alice.localhost'>
            <query xmlns='http://jabber.org/protocol/disco#info'>
                <identity type='jabber:last' 
                          name='Last Component' 
                          category='component'/>
                <feature var='jabber:iq:last'/>
            </query>
        </iq>
    ">>),
    ?try_catch_xml(Reply, 1000),
    finish().

disco_test(_Config) ->
    init(disco_test),
    Packet = #received_packet{
        packet_type=iq, type_attr="get", raw_packet=
            ?Parse(<<"
                <iq xmlns='jabber:client'
                    type='get'
                    to='alice.localhost'
                    id='test_bot'>
                    <query xmlns='http://jabber.org/protocol/disco#info'/>
                </iq>
            ">>),
        from={"bob","localhost",undefined}
    },
    Pid = self(),
    meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
        Pid ! P
    end),
    ecomponent ! Packet,
    Reply = ?CleanXML(<<"
        <iq xmlns='jabber:client'
            type='result'
            id='test_bot'
            from='alice.localhost'>
            <query xmlns='http://jabber.org/protocol/disco#info'>
                <feature var='jabber:iq:last'/>
            </query>
        </iq>
    ">>),
    ?try_catch_xml(Reply, 1000),
    finish().

forward_response_module_test(_Config) ->
    init(forward_response_module_test),
    Id = <<"forward_response_module_test">>,
    Packet = #received_packet{
        packet_type=iq, type_attr="error", raw_packet=
            ?Parse(<<"
                <iq xmlns='jabber:client'
                    type='error'
                    to='alice.localhost'
                    id='", Id/binary, "'>
                    <error xmlns='urn:itself'/>
                </iq>
            ">>),
        from={"bob","localhost",undefined}
    },
    timem:insert(Id, #matching{id="forward_response_module_test", ns='urn:itself', processor=self()}),
    ecomponent ! Packet,
    ?try_catch(#response{ns='urn:itself', params=Params} when is_record(Params,params), 1000),
    finish().

forward_ns_in_set_test(_Config) ->
    init(forward_ns_in_set_test),
    Packet = #received_packet{
        packet_type=iq, type_attr="set", raw_packet=
            ?Parse(<<"
                <iq xmlns='jabber:client'
                    type='set'
                    to='alice.localhost'
                    id='test_fwns_set'>
                    <data xmlns='urn:itself'/>
                </iq>
            ">>),
        from={"bob", "localhost", undefined}
    },
    Pid = self(),
    meck:expect(dummy, process_iq, fun(Params) ->
        error_logger:info_msg("Received params: ~p~n", [Params]),
        Pid ! Params
    end),
    ecomponent ! Packet,
    finish().

save_id_expired_test(_Config) ->
    init(save_id_expired_test),
    Id = ecomponent:gen_id(),
    Packet = ?Parse(<<"
        <iq xmlns='jabber:client'
            type='set'
            to='alice.localhost'
            id='", Id/binary, "'>
            <data xmlns='urn:itself'/>
        </iq>
    ">>),
    Pid = self(),
    meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
        Pid ! P
    end),
    ecomponent:save_id(Id, 'urn:itself', Packet, dummy),
    ecomponent ! getup, %% for init counter
    Reply = ?CleanXML(<<"
        <iq xmlns='jabber:client'
            type='set'
            to='alice.localhost'
            id='", Id/binary, "'>
            <data xmlns='urn:itself'/>
        </iq>
    ">>),
    ?try_catch_xml(Reply, 3000),
    finish().

coutdown_test(_Config) ->
    init(countdown_test),
    St = {state, 
        undefined, undefined, undefined, undefined, undefined, 
        undefined, undefined, undefined, undefined, undefined,
        undefined, undefined, undefined, 3, undefined, undefined,
        undefined, undefined, undefined, [], [], true
    },
    ?assertEqual(100, ecomponent:get_countdown(St)),
    State = ecomponent:reset_countdown(St),
    timer:sleep(1000),
    2000 = ecomponent:get_countdown(State),
    timer:sleep(1000),
    1000 = ecomponent:get_countdown(State),
    timer:sleep(1000),
    100 = ecomponent:get_countdown(State),
    finish().

access_list_get_test(_Config) ->
    init(access_list_get_test),
    Bob1 = { "", "bob1.localhost", "" },
    true = gen_server:call(ecomponent, {access_list_get, 'com.ecomponent.ns/ns1', Bob1}),
    finish().

access_list_set_test(_Config) ->
    init(access_list_set_test),
    Bob = { "", "bob.localhost", "" },
    Bob1 = { "", "bob1.localhost", "" },
    true = gen_server:call(ecomponent, {access_list_set, 'com.ecomponent.ns/ns1', Bob}),
    false = gen_server:call(ecomponent, {access_list_set, 'com.ecomponent.ns/ns1', Bob1}),
    finish().
