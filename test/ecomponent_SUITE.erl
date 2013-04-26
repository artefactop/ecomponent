-module(ecomponent_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/ecomponent.hrl").

-export([suite/0, all/0]).
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    config_test/1, ping_test/1, disco_test/1,
    forward_response_module_test/1,forward_ns_in_set_test/1,
    save_id_expired_test/1, coutdown_test/1,
    message_test/1, presence_test/1, access_list_get_test/1,
    access_list_set_test/1, disco_muted_test/1
]).

suite() ->
    [{ct_hooks,[{cth_junit, [{path, "junit_ecomponent.xml"}]}]},{timetrap,{seconds,30}}].

all() -> 
    [
        disco_muted_test, config_test, ping_test, disco_test, forward_response_module_test,
        forward_ns_in_set_test, save_id_expired_test, coutdown_test,
        message_test, presence_test, access_list_get_test, access_list_set_test
    ].

init_per_suite(Config) ->
    mnesia:start(),
    meck:new(lager, [no_link]),
    meck:expect(lager, info, fun(X,Y) -> error_logger:info_msg(X,Y) end),
    meck:expect(lager, info, fun(X) -> error_logger:info_msg(X) end),
    meck:expect(lager, dispatch_log, fun(_Severity, _Metadata, Format, Args, _Size) ->
        error_logger:info_msg(Format, Args)
    end),
    meck:expect(lager, dispatch_log, fun(_Severity, _Module, _Function, _Line, _Pid, _Traces, Format, Args, _TruncSize) ->
        error_logger:info_msg(Format, Args)
    end),
    
    error_logger:info_msg("INIT SUITE"),
    
    meck:new(syslog, [no_link]),
    meck:expect(syslog, open, fun(_Name, _Opts, _Facility) -> ok end),
    
    meck:new(confetti, [no_link]),
    
    meck:new(exmpp_component, [no_link]),
    meck:expect(exmpp_component, start, fun() -> self() end),
    meck:expect(exmpp_component, stop, fun(_) -> ok end),
    meck:expect(exmpp_component, auth, fun(_Pid, _JID, _Pass) -> ok end),
    meck:expect(exmpp_component, connect, fun(_Pid, _Server, _Port) -> "1234" end),
    meck:expect(exmpp_component, handshake, fun(_Pid) -> ok end),
    application:start(exmpp),
    Config.

end_per_suite(_Config) ->
    error_logger:info_msg("END SUITE"),
    mnesia:stop(),
    application:stop(exmpp),
    meck:unload(syslog),
    meck:unload(confetti),
    meck:unload(exmpp_component),
    meck:unload(lager),
    ok.

init_per_testcase(config_test, Config) ->
    Config;
init_per_testcase(disco_muted_test, Config) ->
    meck:expect(confetti, fetch, fun(_) -> [[{ecomponent, [
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
    ]}]] end),
    {ok, _Pid} = ecomponent:start_link(),
    Config;
init_per_testcase(save_id_expired_test, Config) ->
    meck:expect(confetti, fetch, fun(_) -> [[{ecomponent, [
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
    ]}]] end),
    {ok, _Pid} = ecomponent:start_link(),
    Config;
init_per_testcase(_, Config) ->
    error_logger:info_msg("INIT GENERIC TESTCASE"),
    meck:expect(confetti, fetch, fun(_) -> [[{ecomponent, [
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
    ]}]] end),
    {ok, _Pid} = ecomponent:start_link(),
    Config.

end_per_testcase(config_test, _Config) ->
    ok;
end_per_testcase(_, _Config) ->
    error_logger:info_msg("END GENERIC TESTCASE"),
    ok = ecomponent:stop().

config_test(_Config) ->
    {ok, State} = ecomponent:init([]),
    lager:info("~p~n", [State]),
    Pid = self(),
    {state, 
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
        ], [], local7, "ecomponent", _Timestamp, _Features, _DiscoInfo} = State,
    ok.

message_test(_Config) ->
    Packet = #received_packet{
        packet_type=message, type_attr="chat", raw_packet=
            {xmlel, 'jabber:client', none, 'message',[
                {<<"type">>,"chat"},
                {<<"to">>,"alice.localhost"},
                {<<"id">>,"test_bot"}
            ], [
                {xmlel, undefined, none, 'body', [],[{cdata, "ping"}]}
            ]},
        from={"bob","localhost",undefined}
    },
    Pid = self(),
    meck:new(dummy),
    meck:expect(dummy, process_message, fun(Message) ->
        error_logger:info_msg("Received message: ~p~n", [Message]),
        Pid ! Message
    end),
    ecomponent ! Packet,
    receive
        #message{type="chat", xmlel=Xmlel} ->
            error_logger:info_msg("Message xmlel: ~p~n", [Xmlel]), 
            ok;
        Any ->
            throw(Any)
    after 1000 ->
        throw("ERROR timeout")
    end.

presence_test(_Config) ->
    Packet = #received_packet{
        packet_type=presence, type_attr=undefined, raw_packet=
            {xmlel, 'jabber:client', none, 'presence',[
                {<<"to">>,"alice.localhost"},
                {<<"id">>,"test_bot"}
            ], []},
        from={"bob","localhost",undefined}
    },
    Pid = self(),
    meck:new(dummy),
    meck:expect(dummy, process_presence, fun(Presence) ->
        error_logger:info_msg("Received presence: ~p~n", [Presence]),
        Pid ! Presence
    end),
    ecomponent ! Packet,
    receive
        #presence{xmlel=Xmlel} ->
            error_logger:info_msg("Presence xmlel: ~p~n", [Xmlel]), 
            ok;
        Any ->
            throw(Any)
    after 1000 ->
        throw("ERROR timeout")
    end.

disco_muted_test(_Config) ->
    DiscoPacket = #received_packet{
        packet_type=iq, type_attr="get", raw_packet=
            {xmlel, 'jabber:client', none, 'iq',[
                {<<"type">>,"get"},
                {<<"to">>,"ecomponent.test"},
                {<<"id">>,"test_bot"}
            ], [
                {xmlel, 'http://jabber.org/protocol/disco#info', none, 'query', [],[]}
            ]},
        from={"bob","localhost",undefined}
    },
    Packet = #received_packet{
        packet_type=iq, type_attr="get", raw_packet=
            {xmlel, 'jabber:client', none, 'iq',[
                {<<"type">>,"get"},
                {<<"to">>,"alice.localhost"},
                {<<"id">>,"test_bot"}
            ], [
                {xmlel, 'urn:xmpp:ping', none, 'ping', [],[]}
            ]},
        from={"bob","localhost",undefined}
    },
    Pid = self(),
    meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
        error_logger:info_msg("Sending Packet: ~p", [P]),
        Pid ! P
    end),
    ecomponent ! DiscoPacket,
    ecomponent ! Packet,
    receive
        {xmlel,'jabber:client',[],iq,[
            {<<"type">>,"result"},
            {<<"id">>,"test_bot"},
            {<<"from">>,"alice.localhost"}
        ],[]} -> 
            ok;
        Any ->
            throw(Any)
    after 1000 ->
        throw("ERROR timeout")
    end.

ping_test(_Config) ->
    Packet = #received_packet{
        packet_type=iq, type_attr="get", raw_packet=
            {xmlel, 'jabber:client', none, 'iq',[
                {<<"type">>,"get"},
                {<<"to">>,"alice.localhost"},
                {<<"id">>,"test_bot"}
            ], [
                {xmlel, 'urn:xmpp:ping', none, 'ping', [],[]}
            ]},
        from={"bob","localhost",undefined}
    },
    Pid = self(),
    meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
        error_logger:info_msg("Sending Packet: ~p", [P]),
        Pid ! P
    end),
    ecomponent ! Packet,
    receive
        {xmlel,'jabber:client',[],iq,[
            {<<"type">>,"result"},
            {<<"id">>,"test_bot"},
            {<<"from">>,"alice.localhost"}
        ],[]} -> 
            ok;
        Any ->
            throw(Any)
    after 1000 ->
        throw("ERROR timeout")
    end.

disco_test(_Config) ->
    Packet = #received_packet{
        packet_type=iq, type_attr="get", raw_packet=
            {xmlel, 'jabber:client', none, 'iq',[
                {<<"type">>,"get"},
                {<<"to">>,"alice.localhost"},
                {<<"id">>,"test_bot"}
            ], [
                {xmlel, 'http://jabber.org/protocol/disco#info', none, 'query', [],[
                    {xmlel, undefined, none, 'feature', [
                        {<<"var">>, <<"jabber:iq:last">>}
                    ], []}
                ]}
            ]},
        from={"bob","localhost",undefined}
    },
    Pid = self(),
    meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
        error_logger:info_msg("Sending Packet: ~p", [P]),
        Pid ! P
    end),
    ecomponent ! Packet,
    receive
        {xmlel,'jabber:client',[],iq,[
            {<<"type">>,"result"},
            {<<"id">>,"test_bot"},
            {<<"from">>,"alice.localhost"}
        ],[
            {xmlel, 'http://jabber.org/protocol/disco#info', [], 'query', [],[
                {xmlel, undefined, [], 'feature', [
                    {xmlattr, undefined, <<"var">>, <<"jabber:iq:last">>}
                ], []}
            ]}
        ]} -> 
            ok;
        Any ->
            throw(Any)
    after 1000 ->
        throw("ERROR timeout")
    end.

forward_response_module_test(_Config) ->
    Id = <<"forward_response_module_test">>,
    Packet = #received_packet{
        packet_type=iq, type_attr="error", raw_packet=
            {xmlel, 'jabber:client', none, 'iq',[
                {<<"type">>,"error"},
                {<<"to">>,"alice.localhost"},
                {<<"id">>,binary_to_list(Id)}
            ], [
                {xmlel, 'urn:itself', none, 'error', [],[]}
            ]},
        from={"bob","localhost",undefined}
    },
    timem:insert(Id, #matching{id="forward_response_module_test", ns='urn:itself', processor=self()}),
    ecomponent ! Packet,
    receive
        #response{ns='urn:itself', params=Params} when is_record(Params,params) ->
            error_logger:info_msg("Params: ~p~n", [Params]),
            ok;
        Any ->
            throw(Any)
    after 1000 ->
        throw("ERROR timeout")
    end.

forward_ns_in_set_test(_Config) ->
    Packet = #received_packet{
        packet_type=iq, type_attr="set", raw_packet=
            {xmlel, 'jabber:client', none, 'iq', [
                {<<"type">>, "set"},
                {<<"to">>, "alice.localhost"},
                {<<"id">>, "test_fwns_set"}
            ], [
                {xmlel, 'urn:itself', none, 'data', [], []}
            ]},
        from={"bob", "localhost", undefined}
    },
    Pid = self(),
    meck:new(dummy),
    meck:expect(dummy, process_iq, fun(Params) ->
        error_logger:info_msg("Received params: ~p~n", [Params]),
        Pid ! Params
    end),
    ecomponent ! Packet,
    receive
        #params{type="set",ns='urn:itself'}=Params ->
            error_logger:info_msg("Params: ~p~n", [Params]),
            ok;
        Any ->
            throw(Any)
    after 1000 ->
        throw("ERROR timeout")
    end.

save_id_expired_test(_Config) ->
    Id = ecomponent:gen_id(),
    Id_l = binary_to_list(Id),
    Packet = {xmlel, 'jabber:client', none, 'iq', [
        {<<"type">>, "set"},
        {<<"to">>, "alice.localhost"},
        {<<"id">>, Id_l}
    ], [
        {xmlel, 'urn:itself', none, 'data', [], []}
    ]},
    Pid = self(),
    meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
        error_logger:info_msg("Sending Packet: ~p", [P]),
        Pid ! P
    end),
    ecomponent:save_id(Id, 'urn:itself', Packet, dummy),
    ecomponent ! getup, %% for init counter
    receive
        {xmlel,'jabber:client',none,iq, [
            {<<"type">>,"set"},
            {<<"to">>,"alice.localhost"},
            {<<"id">>,Id_l}
        ], [
            {xmlel,'urn:itself',none,data,[],[]}
        ]} ->
            ok;
        Any ->
            error_logger:info_msg("~p~n", [Any]),
            throw(Any)
    after 3000 ->
        throw("Timeout error")
    end.

coutdown_test(_Config) ->
    St = {state, 
        undefined, undefined, undefined, undefined, undefined, 
        undefined, undefined, undefined, undefined, undefined,
        undefined, undefined, undefined, 3, undefined, undefined,
        undefined, undefined, undefined, [], true
    },
    100 = ecomponent:get_countdown(St),
    State = ecomponent:reset_countdown(St),
    timer:sleep(1000),
    2000 = ecomponent:get_countdown(State),
    timer:sleep(1000),
    1000 = ecomponent:get_countdown(State),
    timer:sleep(1000),
    100 = ecomponent:get_countdown(State),
    ok.

access_list_get_test(_Config) ->
    Bob1 = { "", "bob1.localhost", "" },
    true = gen_server:call(ecomponent, {access_list_get, 'com.ecomponent.ns/ns1', Bob1}).

access_list_set_test(_Config) ->
    Bob = { "", "bob.localhost", "" },
    Bob1 = { "", "bob1.localhost", "" },
    true = gen_server:call(ecomponent, {access_list_set, 'com.ecomponent.ns/ns1', Bob}),
    false = gen_server:call(ecomponent, {access_list_set, 'com.ecomponent.ns/ns1', Bob1}).

