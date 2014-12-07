% snippets for tests

% required for eunit to work
-include_lib("eunit/include/eunit.hrl").
-include("ecomponent_internal.hrl").

-record(mockup, {
    module :: atom(),
    function :: atom(),
    code :: function()
}).

-record(step, {
    name = <<"noname">> :: binary(),
    type = 'send' :: 'send' | 'receive',
    times = 1 :: pos_integer(),
    timeout = 1000 :: pos_integer(),
    stanza :: exmpp_xml:xmlel(),
    idserver = default :: atom()
}).

-type mockup() :: #mockup{}.
-type step() :: #step{}.

-record(functional, {
    mockups = [] :: [mockup()],
    mock_opts = [] :: [atom()],
    steps = [] :: [step()],
    config = [] :: [term()],
    start = fun() -> ok end :: function(),
    stop = fun() -> ok end :: function()
}).

-type functional() :: #functional{}.

-define(run_exmpp(), begin
    case lists:keyfind(exmpp, 1, application:loaded_applications()) of
        false ->
            application:start(exmpp);
        _ ->
            ok
    end
end).

-define(meck_lager(Verbose), begin
    meck:new(lager),
    meck:expect(lager, dispatch_log, fun(_Severity, _Module, _Function, _Line, _Pid, _Traces, _Format, _Args, _TruncSize) ->
        if
            Verbose ->
                ?debugFmt(_Format, _Args),
                ok;
            true -> ok
        end
    end)
end).

-define(meck_lager(), ?meck_lager(false)).

-define(meck_syslog(Verbose), begin
    meck:new(syslog),
    meck:expect(syslog, open, fun(_Name, _Opts, _Facility) -> ok end),
    meck:expect(syslog, log, fun(_Level, _Message) ->
        if
            Verbose ->
                ?debugFmt("~p: ~s~n", [_Level,_Message]),
                ok;
            true -> ok
        end
    end),
    meck:new(ecomponent, [passthrough]),
    meck:expect(ecomponent, syslog, fun(_A,_B) -> 
        if
            Verbose ->
                ?debugFmt("~p: ~s~n", [_A,_B]),
                ok;
            true -> ok
        end
    end)
end).

-define(meck_syslog(), ?meck_syslog(false)).

-define(meck_config(Config), begin
    lists:foreach(fun
        ({Key, Val}) ->
            application:set_env(ecomponent, Key, Val);
        %% dummy added in the configuration:
        ([]) -> ok
    end, Config)
end).

-define(meck_component(), (fun() ->
    meck:new(exmpp_component),
    meck:expect(exmpp_component, start, fun() -> self() end),
    meck:expect(exmpp_component, stop, fun(_) -> ok end),
    meck:expect(exmpp_component, auth, fun(_Pid, _JID, _Pass) -> ok end),
    meck:expect(exmpp_component, connect, fun(_Pid, _Server, _Port) -> "1234" end),
    meck:expect(exmpp_component, handshake, fun(_Pid) -> ok end),

    meck:new(ecomponent_con_sup),
    meck:expect(ecomponent_con_sup, start_child, fun(_,_,_) -> ok end)
end)()).

-define(meck_metrics(), begin 
    meck:new(ecomponent_metrics),
    meck:expect(ecomponent_metrics, init, 0, ok),
    meck:expect(ecomponent_metrics, init, 1, ok),
    meck:expect(ecomponent_metrics, notify_throughput_iq, 3, ok),
    meck:expect(ecomponent_metrics, notify_throughput_presence, 2, ok),
    meck:expect(ecomponent_metrics, notify_throughput_message, 2, ok),
    meck:expect(ecomponent_metrics, set_iq_time, 3, ok),
    meck:expect(ecomponent_metrics, notify_dropped_iq, 2, ok),
    meck:expect(ecomponent_metrics, notify_dropped_presence, 1, ok),
    meck:expect(ecomponent_metrics, notify_dropped_message, 1, ok),
    meck:expect(ecomponent_metrics, notify_resp_time, 1, ok),
    meck:expect(ecomponent_metrics, notify, 1, ok),
    meck:expect(ecomponent_metrics, notify, 2, ok)
end).

-define(meck_jid(), begin
    meck:new(exmpp_jid),
    meck:expect(exmpp_jid, parse, fun
        (String) when is_list(String) ->
            exmpp_jid:parse(list_to_binary(String));
        (String) when is_binary(String) ->
            case binary:split(String, <<"@">>) of 
                [Component] ->
                    case binary:split(Component, <<"/">>) of
                        [Component] ->
                            #jid{
                                raw=String,
                                domain=Component
                            };
                        [ComponentSimple, Resource] ->
                            #jid{
                                raw=String,
                                domain=ComponentSimple,
                                resource=Resource
                            }
                    end;
                [User,Server] ->
                    case binary:split(Server, <<"/">>) of
                        [ServerSimple, Resource] ->
                            #jid{
                                raw=String,
                                node=User,
                                domain=ServerSimple,
                                resource=Resource
                            };
                        [Server] ->
                            #jid{
                                raw=String,
                                node=User,
                                domain=Server
                            }
                    end
            end
    end),
    meck:expect(exmpp_jid, node, fun(JID) -> JID#jid.node end),
    meck:expect(exmpp_jid, prep_bare_to_binary, fun
        (#jid{node=N,domain=D}) -> 
            <<N/binary, "@", D/binary>> 
    end),
    meck:expect(exmpp_jid, to_list, fun(Node, Domain) ->
        binary_to_list(<<Node/binary, "@", Domain/binary>>)
    end)
end).

-define(no_try_catch(NoMatch, Timeout),
    (fun() -> receive NoMatch=NM -> ?debugFmt("match: ~p~n", [NM]), throw("Matching!"); _ -> ok after Timeout -> ok end end)()
).

-define(try_catch_match(Match, Timeout),
    (fun() -> receive Match; Any -> throw(Any) after Timeout -> throw("TIMEOUT") end end)()
).

-define(try_catch(Match, Timeout), 
    (fun() -> receive Match -> ok; Any -> throw(Any) after Timeout -> throw("TIMEOUT") end end)()
).

-define(try_catch_xml(Match, Timeout),
    (fun() -> receive M -> case ?ToXML(M) of Match -> ok; Any -> ?debugFmt("value: ~p", [Any]), ?debugFmt("expected: ~p", [Match]), throw(Any) end after Timeout -> throw("TIMEOUT") end end)()
).

-define(Parse(XML), begin
    exmpp_xml:remove_whitespaces_deeply(lists:nth(1, exmpp_xml:parse_document(XML)))  
end). 

-define(CleanXML(XML), ?ToXML(?Parse(XML))).

-define(ToXML(Stanza), begin
    binary:replace(
        exmpp_xml:document_to_binary(Stanza),
        <<" xmlns=\"\"">>,
        <<>>,
        [global])
end).
