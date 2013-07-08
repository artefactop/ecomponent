-module(ecomponent_test).

-compile(export_all).

-include("../include/ecomponent_test.hrl").

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
            processor_presence_test(Config)
        ] end
    }.

init_per_suite() ->
    mnesia:start(),
    ?meck_lager(),
    ?meck_syslog(),
    ?meck_component(),
    ?meck_metrics(),
    ?run_exmpp(),
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
    meck:new(ecomponent_sup),
    meck:expect(ecomponent_sup, start_child, fun(_,_,_) -> ok end),
    meck:unload(confetti). 

end_per_suite(_Config) ->
    mnesia:stop(),
    meck:unload(),
    ok.

init(multiconnection_test) ->
    Conf = [
        {syslog_name, "ecomponent" },
        {jid, "ecomponent.test" },
        {servers, [
            {server_one, [
                {server, "localhost" },
                {port, 8899},
                {pass, "secret"}
            ]},
            {server_two, [
                {server, "localhost" },
                {port, 8899},
                {pass, "secret"}
            ]}
        ]},
        {whitelist, []}, %% throttle whitelist
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
    ],
    ?meck_confetti([[{ecomponent, Conf}]]),
    meck:new(dummy),
    {ok, _} = ecomponent:start_link(),
    {ok, _} = ecomponent_con_worker:start_link(server_one, "ecomponent.test", [
        {server, "localhost" },
        {port, 8899},
        {pass, "secret"}
    ]),
    {ok, _} = ecomponent_con_worker:start_link(server_two, "ecomponent.test", [
        {server, "localhost" },
        {port, 8899},
        {pass, "secret"}
    ]);
init(disco_info_test) ->
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
        {disco_info, true},
        {info, [
            {type, <<"jabber:last">>},
            {name, <<"Last Component">>}
        ]},
        {features, [<<"jabber:iq:last">>]}
    ],
    ?meck_confetti([[{ecomponent, Conf}]]),
    meck:new(dummy),
    {ok, _} = ecomponent:start_link(),
    {ok, _} = ecomponent_con_worker:start_link(default, "ecomponent.test", Conf);
init(disco_muted_test) ->
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
        {disco_info, false}
    ],
    ?meck_confetti([[{ecomponent, Conf}]]),
    meck:new(dummy),
    {ok, _} = ecomponent:start_link(),
    {ok, _} = ecomponent_con_worker:start_link(default, "ecomponent.test", Conf);
init(save_id_expired_test) ->
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
        {request_timeout, 2}
    ],
    ?meck_confetti([[{ecomponent, Conf}]]),
    meck:new(dummy),
    {ok, _} = ecomponent:start_link(),
    {ok, _} = ecomponent_con_worker:start_link(default, "ecomponent.test", Conf);
init(sync_send_test) ->
    Conf = [
        {syslog_name, "ecomponent" },
        {jid, "ecomponent.test" },
        {server, "localhost" },
        {port, 8899},
        {pass, "secret"},
        {whitelist, [] }, %% throttle whitelist
        {access_list_get, []},
        {access_list_set, []},
        {max_per_period, 15},
        {period_seconds, 8},
        {processors, [
            {default, {mod, dummy}}
        ]}
    ],
    ?meck_confetti([[{ecomponent, Conf}]]),
    meck:new(dummy),
    {ok, _} = ecomponent:start_link(),
    {ok, _} = ecomponent_con_worker:start_link(default, "ecomponent.test", Conf);
init(config_test) ->
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
        {features, [<<"jabber:iq:last">>]},
        {mnesia_callback, [
            {dummy, tables, []}
        ]}
    ]}]]);
init(processor_test) ->
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
            {'jabber:iq:last', {mod, last}}
        ]},
        {disco_info, false}
    ],
    ?meck_confetti([[{ecomponent, Conf}]]),
    meck:new(dummy),
    {ok, _} = ecomponent:start_link(),
    {ok, _} = ecomponent_con_worker:start_link(default, "ecomponent.test", Conf);
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
        {features, [<<"jabber:iq:last">>]}
    ],
    ?meck_confetti([[{ecomponent, Conf}]]),
    meck:new(dummy),
    {ok, _} = ecomponent:start_link(),
    {ok, _} = ecomponent_con_worker:start_link(default, "ecomponent.test", Conf).

-define(finish(), begin
    meck:unload(confetti),
    meck:unload(dummy),
    ecomponent:stop(),
    ?_assert(true)
end).

-record(dummy, {id,name,value}).

config_test(_Config) ->
    init(config_test),
    meck:new(dummy),
    meck:expect(dummy, tables, 0, [{dummy, ram_copies, record_info(fields, dummy)}]), 
    {ok, State} = ecomponent:init([]), 
    meck:unload(dummy), 
    mnesia:table_info(dummy, all), 
    lager:info("~p~n", [State]),
    timer:sleep(250), 
    meck:unload(confetti),
    ?_assertMatch(#state{
        jid = "ecomponent.test",
        maxPerPeriod = 15,
        periodSeconds = 8,
        processors = [{default, {mod, dummy}}],
        message_processor = {mod, dummy},
        presence_processor = {mod, dummy},
        maxTries = 3,
        resendPeriod = 100,
        requestTimeout = 10,
        accessListSet = [
            {'com.ecomponent.ns/ns1', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns2', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns3', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns4', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns5', [<<"bob.localhost">>]},
            {'com.ecomponent.ns/ns6', [<<"bob.localhost">>]}
        ],
        syslogFacility = local7,
        syslogName = "ecomponent"}, State).

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
    ?finish().

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
    ?finish().

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
    ?finish().

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
    ?finish().

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
    ?finish().

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
    ?finish().

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
    ?finish().

forward_ns_in_set_test(_Config) ->
    init(forward_ns_in_set_test),
    Payload = ?Parse(<<"
        <data xmlns='urn:itself'/>
    ">>),
    IQ = ?Parse(<<"
        <iq xmlns='jabber:client'
            type='set'
            to='alice.localhost'
            id='test_fwns_set'>
            <data xmlns='urn:itself'/>
        </iq>
    ">>),
    Packet = #received_packet{
        packet_type=iq, type_attr="set", raw_packet=IQ,
        from={"bob", "localhost", undefined}
    },
    Pid = self(),
    meck:expect(dummy, process_iq, fun(Params) ->
        %?debugFmt("Received params: ~p~n", [Params]),
        Pid ! Params
    end),
    ecomponent ! Packet,
    ?try_catch(#params{
        type="set", from={"bob","localhost",undefined},
        to={undefined,<<"alice.localhost">>,undefined},
        ns='urn:itself',
        payload = Payload,
        iq = IQ,
        features=[<<"jabber:iq:last">>],
        info=[]
    }, 1000),
    ?finish().

save_id_expired_test(_Config) ->
    init(save_id_expired_test),
    Id = ecomponent:gen_id(),
    Packet = ?Parse(<<"
        <iq xmlns='jabber:client'
            type='set'
            from='bob@localhost/res'
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
            from='bob@localhost/res'
            to='alice.localhost'
            id='", Id/binary, "'>
            <data xmlns='urn:itself'/>
        </iq>
    ">>),
    ?try_catch_xml(Reply, 3000),
    ?finish().

sync_send_test(_Config) ->
    init(sync_send_test),
    Send_Packet = ?Parse(<<"
        <iq xmlns='jabber:client'
            type='get'
            from='alice.localhost'
            to='bob.localhost'
            id='test_bot'>
            <query xmlns='http://jabber.org/protocol/disco#info'/>
        </iq>
    ">>),
    NS = 'http://jabber.org/protocol/disco#info',
    Pid = self(),
    meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
        Pid ! P
    end),
    spawn(fun() -> 
        Pid ! ecomponent:sync_send(Send_Packet, NS) 
    end),
    Packet = #received_packet{
        packet_type=iq, type_attr="result", raw_packet=
            ?Parse(<<"
                <iq xmlns='jabber:client'
                    type='result'
                    id='test_bot'
                    from='bob.localhost'
                    to='alice.localhost'>
                    <query xmlns='http://jabber.org/protocol/disco#info'>
                        <feature var='jabber:iq:last'/>
                    </query>
                </iq>
            ">>),
        queryns=NS,
        from={undefined,"bob.localhost",undefined}
    },
    ecomponent ! Packet,
    SendXML = ?ToXML(Send_Packet),
    ?try_catch_xml(SendXML, 1000),
    Params = #params{
        type = "result",
        from = {undefined,"bob.localhost",undefined},
        to = {undefined, <<"alice.localhost">>, undefined},
        ns=NS,
        payload = ?Parse(<<"
            <query xmlns='http://jabber.org/protocol/disco#info'>
                <feature var='jabber:iq:last'/>
            </query>
        ">>),
        iq = ?Parse(<<"
            <iq xmlns='jabber:client'
                type='result'
                id='test_bot'
                from='bob.localhost'
                to='alice.localhost'>
                <query xmlns='http://jabber.org/protocol/disco#info'>
                    <feature var='jabber:iq:last'/>
                </query>
            </iq>
        ">>)
    },
    ?try_catch(Params, 1000),
    ?finish().

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
    true = gen_server:call(ecomponent, {access_list_get, 'com.ecomponent.ns/ns1', Bob1}),
    ?finish().

access_list_set_test(_Config) ->
    init(access_list_set_test),
    Bob = {undefined, "bob.localhost", undefined},
    Bob1 = {undefined, "bob1.localhost", undefined},
    true = gen_server:call(ecomponent, {access_list_set, 'com.ecomponent.ns/ns1', Bob}),
    false = gen_server:call(ecomponent, {access_list_set, 'com.ecomponent.ns/ns1', Bob1}),
    ?finish().

multiconnection_test(_Config) ->
    init(multiconnection_test),
    Packet = #received_packet{
        packet_type=iq, type_attr="get", raw_packet=
            ?Parse(<<"
                <iq xmlns='jabber:client'
                    type='get'
                    from='bob@localhost/res'
                    to='alice.localhost'
                    id='test_bot'>
                    <query xmlns='http://jabber.org/protocol/disco#info'/>
                </iq>
            ">>),
        from={"bob","localhost",undefined}
    },
    Pid = self(),
    meck:new(timem),
    meck:expect(timem, insert, 2, ok),
    meck:expect(timem, remove, fun
        (ID) when is_tuple(ID) ->
            Pid ! ID,
            server_two
    end),
    meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
        Pid ! P
    end),
    server_two ! Packet,
    ?try_catch({<<"test_bot">>, <<"bob@localhost">>}, 1000),
    Reply = ?CleanXML(<<"
        <iq xmlns='jabber:client'
            type='result'
            from='alice.localhost'
            to='bob@localhost/res'
            id='test_bot'>
            <query xmlns='http://jabber.org/protocol/disco#info'>
                <identity type='jabber:last'
                          name='Last Component'
                          category='component'/>
                <feature var='jabber:iq:last'/>
            </query>
        </iq>
    ">>),
    ?try_catch_xml(Reply, 1000),
    meck:unload(timem), 
    meck:unload(confetti),
    meck:unload(dummy),
    ecomponent:stop(),
    ?_assert(true).

processor_iq_test(_Config) ->
    init(processor_test),
    Packet = #received_packet{
        packet_type=iq, type_attr="get", raw_packet=
            ?Parse(<<"
                <iq xmlns='jabber:client'
                    type='get'
                    from='bob@localhost/res'
                    to='alice.localhost'
                    id='test_bot'>
                    <query xmlns='whatever'/>
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
            type='error'
            from='alice.localhost'
            to='bob@localhost/res'
            id='test_bot'>
            <query xmlns='whatever'/>
            <error type='cancel'>
                <service-unavailable xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
            </error>
        </iq>
    ">>),
    ?try_catch_xml(Reply, 1000),
    ?finish().

processor_message_test(_Config) ->
    init(processor_test),
    Packet = #received_packet{
        packet_type=message, type_attr="chat", raw_packet=
            ?Parse(<<"
                <message
                    type='chat'
                    from='bob@localhost/res'
                    to='alice.localhost'
                    id='test_bot'>
                    <body/>
                </message>
            ">>),
        from={"bob","localhost",undefined}
    },
    ecomponent ! Packet,
    ?finish().

processor_presence_test(_Config) ->
    init(processor_test),
    Packet = #received_packet{
        packet_type=presence, type_attr="unavailable", raw_packet=
            ?Parse(<<"
                <presence
                    type='unavailable'
                    from='bob@localhost/res'
                    to='alice.localhost'
                    id='test_bot'/>
            ">>),
        from={"bob","localhost",undefined}
    },
    ecomponent ! Packet,
    ?finish().
