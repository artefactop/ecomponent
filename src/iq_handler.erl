-module(iq_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("ecomponent.hrl").

%% API
-export([pre_process_iq/7]).

-spec pre_process_iq( 
    Type::undefined | string(), 
    NS::atom(), 
    IQ::term(), 
    From::ecomponent:jid(), 
    Features::[binary()], 
    Info::proplists:proplists(),
    ServerID::atom()) -> ok.
%@doc Pre-process the IQ stanza. This process get more information from
%     the stanza and then send in a params record to the process_iq/1.
%@end
pre_process_iq(Type, IQ, NS, From, Features, Info, ServerID) ->
    Payload = exmpp_iq:get_payload(IQ),
    To = exmpp_jid:to_lower(exmpp_stanza:get_recipient(IQ)),
    process_iq(#params{
        from=From, to=To, ns=NS, type=Type, 
        iq=IQ, payload=Payload, features=Features, 
        info=Info, server=ServerID}).

-spec process_iq( Params::#params{} ) -> ok.
%@doc Do the basic process IQ. If the stanza is a ping or disco#info,
%     the stanza should be attendee instead of passing to the module
%     configured in the configfile.
%@end
process_iq(#params{type="get", iq=IQ, ns=?NS_PING}) ->
    Result = exmpp_iq:result(IQ),
    ecomponent:send(Result, ?NS_PING, undefined);

process_iq(#params{type="get", to={undefined,_,_}, iq=IQ, ns=?NS_DISCO_INFO, features=Features, info=Info}) ->
    Identity = case {
        proplists:get_value(type, Info),
        proplists:get_value(name, Info),
        proplists:get_value(category, Info, <<"component">>)
    } of 
        {undefined, _, _} -> [];
        {_, undefined, _} -> [];
        {undefined, undefined, _} -> [];
        {Type, Name, Category} ->
            [exmpp_xml:element(?NS_DISCO_INFO, 'identity', [
                exmpp_xml:attribute(<<"type">>, Type),
                exmpp_xml:attribute(<<"name">>, Name),
                exmpp_xml:attribute(<<"category">>, Category) 
            ], [])]
    end,
    Result = exmpp_iq:result(IQ, exmpp_xml:element(?NS_DISCO_INFO, 'query', [],
        Identity ++ 
        lists:map(fun(Feature) ->
            exmpp_xml:element(undefined, 'feature', [
                exmpp_xml:attribute(<<"var">>, Feature) 
            ], [])  
        end, Features)
    )),
    ecomponent:send(Result, ?NS_DISCO_INFO, undefined);

process_iq(#params{type="error"}=Params) ->
    forward_response(Params);

process_iq(#params{type="result"}=Params) ->
    forward_response(Params);

process_iq(#params{type="set", ns=NS, iq=IQ, from=From}=Params) ->
    case ecomponent_acl:access_list_set(NS, From) of
    true ->
        forward_ns(Params);
    false ->
        ecomponent:syslog(warning, io_lib:format("Access denied for: ~p to ~p~n", [From, NS])),
        ecomponent:send(exmpp_iq:error(IQ, 'forbidden'), NS, undefined)
    end;

process_iq(#params{type="get", ns=NS, iq=IQ, from=From}=Params) ->
    case ecomponent_acl:access_list_get(NS, From) of
    true ->
        forward_ns(Params);
    false ->
        ecomponent:syslog(warning, io_lib:format("Access denied for: ~p to ~p~n", [From, NS])),
        ecomponent:send(exmpp_iq:error(IQ, 'forbidden'), NS, undefined)
    end;

process_iq(P) ->
    lager:info("Unknown Request: ~p~n", [P]).

-spec forward_ns( Params::#params{} ) -> ok.
%@doc Do the forward based on namespace.
%@end
forward_ns(#params{ns=NS}=Params) ->
    case ecomponent:get_processor_by_ns(NS) of
    undefined -> 
        processor:process_iq(Params);
    {mod, P} ->
        try
            P:process_iq(Params)
        catch Error ->
            lager:error("call to module ~p die: ~p~n", [P, Error]),
            Error = exmpp_xml:element(undefined, 'error', [
                exmpp_xml:attribute(<<"type">>, <<"wait">>),
                exmpp_xml:attribute(<<"code">>, <<"500">>)
            ], [
                exmpp_xml:element(
                    'urn:ietf:params:xml:ns:xmpp-stanzas', 
                    'internal-server-error', [], []),
                exmpp_xml:element(
                    'urn:ietf:params:xml:ns:xmpp-stanzas',
                    'text', [], [
                        exmpp_xml:cdata(<<"Server crash!">>)
            ])]),
            Result = exmpp_iq:error(Params#params.iq, Error),
            ecomponent:send(Result, ?NS_DISCO_ITEMS, ecomponent, false)
        end;
    {app, Name} ->
        PID = whereis(Name),
        case erlang:is_pid(PID) andalso erlang:is_process_alive(PID) of
        true -> 
            PID ! {iq, Params};
        _ -> 
            lager:warning("Process not Alive for NS: ~p~n", [NS])
        end;
    Proc -> 
        lager:warning("Unknown Request to Forward: ~p ~p~n", [Proc, Params])
    end.

-spec forward_response( Params::#params{} ) -> ok.
%@doc Forward a response based on return path. If the IQ stanza is error or 
%     result type the system search based on ID to send the response to the
%     correct application or process.
%@end
forward_response(#params{iq=IQ}=Params) ->
    ID = exmpp_stanza:get_id(IQ),
    case ecomponent:get_processor(ID) of
    #matching{ns=NS, processor=App} ->
        App ! #response{ns=NS, params=Params},
        ok;
    _ ->
        ok
    end.
