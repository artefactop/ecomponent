-module(iq_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/ecomponent.hrl").

%% API
-export([pre_process_iq/3]).

-spec pre_process_iq( (get | set | error | result), IQ::term(), From::ecomponent:jid()) -> ok.

pre_process_iq(Type, IQ, From) ->
    Payload = exmpp_iq:get_payload(IQ),
    case Payload of
        undefined ->
            NS = undefined;
        _ ->
            NS = exmpp_xml:get_ns_as_atom(Payload)
    end,
    process_iq(#params{from=From, ns=NS, type=Type, iq=IQ, payload=Payload}).

-spec process_iq( Params::#params{} ) -> ok.

process_iq(#params{type="get", iq=IQ, ns=?NS_PING}) ->
    Result = exmpp_iq:result(IQ),
    ecomponent:send(Result, ?NS_PING, undefined);

process_iq(#params{type="get", iq=IQ, ns=?NS_DISCO_INFO}) ->
    Result = exmpp_iq:result(IQ, exmpp_xml:element(?NS_DISCO_INFO, 'query', [], [])),
    ecomponent:send(Result, ?NS_DISCO_INFO, undefined);

process_iq(#params{type="error"}=Params) ->
    forward_response(Params);

process_iq(#params{type="result"}=Params) ->
    forward_response(Params);

process_iq(#params{type="set", ns=NS, iq=IQ, from=From}=Params) ->
    case gen_server:call(ecomponent, {access_list_set, NS, From}) of
        true ->
            forward_ns(Params);
        false ->
            ecomponent:syslog(warning, io_lib:format("Access denied for: ~p to ~p~n", [From, NS])),
            ecomponent:send(exmpp_iq:error(IQ, 'bad-request'), NS, undefined)
    end;

process_iq(#params{type="get", ns=NS, iq=IQ, from=From}=Params) ->
    case gen_server:call(ecomponent, {access_list_set, NS, From}) of
        true ->
            forward_ns(Params);
        false ->
            ecomponent:syslog(warning, io_lib:format("Access denied for: ~p to ~p~n", [From, NS])),
            ecomponent:send(exmpp_iq:error(IQ, 'bad-request'), NS, undefined)
    end;

process_iq(P) ->
    lager:info("Unknown Request: ~p~n", [P]).

-spec forward_ns( Params::#params{} ) -> ok.

forward_ns(#params{ns=NS}=Params) ->
    case ecomponent:get_processor_by_ns(NS) of
        undefined -> 
            spawn(?MODULE, handle_unavailable, [Params]);
        {mod, P} ->
            spawn(P, process_iq, [Params]);
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

forward_response(#params{iq=IQ}=Params) ->
    ID = exmpp_stanza:get_id(IQ),
    case ecomponent:get_processor(ID) of
        undefined -> 
            ok;
        #matching{processor=undefined} ->
            ok;
        #matching{ns=NS, processor=App} ->
            PID = whereis(App),
            case is_pid(PID) of 
                true ->
                    PID ! #response{ns=NS, params=Params},
                    ok;
                _ -> ok
            end;
        _ -> 
            ok
    end;

forward_response(_) -> 
    ok.

