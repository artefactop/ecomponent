% snippets for tests

% required for eunit to work
-include_lib("eunit/include/eunit.hrl").
-include("../include/ecomponent_internal.hrl").

-define(run_exmpp(), begin
    case lists:keyfind(exmpp, 1, application:loaded_applications()) of
        false ->
            application:start(exmpp);
        _ ->
            ok
    end
end).

-define(meck_lager(), begin
    meck:new(lager),
    meck:expect(lager, dispatch_log, fun(_Severity, _Metadata, _Format, _Args, _Size) ->
        %?debugFmt(_Format, _Args),
        ok
    end),
    meck:expect(lager, dispatch_log, fun(_Severity, _Module, _Function, _Line, _Pid, _Traces, _Format, _Args, _TruncSize) ->
        %?debugFmt(_Format, _Args),
        ok
    end)
end).

-define(meck_syslog(), begin
    meck:new(syslog),
    meck:expect(syslog, open, fun(_Name, _Opts, _Facility) -> ok end),
    meck:expect(syslog, log, fun(_Level, _Message) ->
        %?debugFmt("~p: ~s~n", [_Level,_Message]),
        ok
    end),
    meck:new(ecomponent, [passthrough]),
    meck:expect(ecomponent, syslog, fun(_A,_B) -> 
        %?debugFmt("~p: ~s~n", [_A,_B]),
        ok
    end)
end).

-define(meck_confetti(Config), begin
    meck:new(confetti),
    meck:expect(confetti, fetch, 1, Config) 
end).

-define(meck_component(), begin 
    meck:new(exmpp_component),
    meck:expect(exmpp_component, start, fun() -> self() end),
    meck:expect(exmpp_component, stop, fun(_) -> ok end),
    meck:expect(exmpp_component, auth, fun(_Pid, _JID, _Pass) -> ok end),
    meck:expect(exmpp_component, connect, fun(_Pid, _Server, _Port) -> "1234" end),
    meck:expect(exmpp_component, handshake, fun(_Pid) -> ok end)
end).

-define(meck_metrics(), begin 
    meck:new(metrics),
    meck:expect(metrics, init, fun() -> ok end),
    meck:expect(metrics, notify_throughput_iq, fun(_IO, _Type, _NS) -> ok end),
    meck:expect(metrics, notify_throughput_presence, fun(_IO, _Type) -> ok end),
    meck:expect(metrics, notify_throughput_message, fun(_IO, _Type) -> ok end),
    meck:expect(metrics, set_iq_time, fun(_ID, _Type, _NS) -> ok end),
    meck:expect(metrics, notify_dropped_iq, fun(_Type, _NS) -> ok end),
    meck:expect(metrics, notify_dropped_presence, fun(_Type) -> ok end),
    meck:expect(metrics, notify_dropped_message, fun(_Type) -> ok end),
    meck:expect(metrics, notify_resp_time, fun(_ID) -> ok end)
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
    (fun() -> receive NoMatch -> throw("Matching!"); _ -> ok after Timeout -> ok end end)()
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
