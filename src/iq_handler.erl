-module(iq_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/ecomponent.hrl").

%% API
-export([pre_process_iq/4]).

pre_process_iq(Type, IQ, From, State) ->
	lager:info("Preparing: ~p~n On State:~p~n", [IQ, State]),
	Payload = exmpp_iq:get_payload(IQ),
	NS = exmpp_xml:get_ns_as_atom(Payload),
	lager:info("NS:~p~n", [NS]),
	process_iq(Type, IQ, #params{from=From, ns=NS, payload=Payload}, State).

process_iq("get", IQ, #params{ns=?NS_PING}, #state{xmppCom=XmppCom}=State) ->
	Result = exmpp_iq:result(IQ),
	ecomponent:send_packet(XmppCom, Result, ?MODULE),
	{ok, State};

process_iq("error"=Type, IQ, #params{}=Params, #state{}=State) ->
	forward_response(Type, IQ, Params, State),
	{ok, State};

process_iq("result"=Type, IQ, #params{}=Params, #state{}=State) ->
	forward_response(Type, IQ, Params, State),
	{ok, State};

process_iq("set"=Type, IQ, #params{}=Params, #state{}=State) ->
	forward_ns(Type, IQ, Params, State),
	{ok, State};

process_iq("get"=Type, IQ, #params{}=Params, #state{}=State) ->
	forward_ns(Type, IQ, Params, State),
	{ok, State};

process_iq(_, IQ, _, State) ->
	lager:info("Unknown Request: ~p~n", [IQ]),
	{ok, State}.

forward_ns(Type, IQ, #params{ns=Ns}=Params, #state{processors=Processors}=State) ->
	lager:info("Choose processor for IQ:~p~n", [IQ]),
	case ecomponent:get_processor_by_ns(Ns, Processors) of
		undefined -> 
			spawn(ns_processor, process_iq, [Type, IQ, Params, State]),
			ok;
		P ->
			lager:info("Processor ~p ~p", [P, Ns]),
			spawn(P, process_iq, [Type, IQ, Params, State]),
			ok
	end.

forward_response(Type, #received_packet{id=Id}=IQ, Params, State) ->
	case ecomponent:get_processor(Id) of
		undefined -> ok;
		P ->
			lager:info("Processor ~p ~s", [P, Id]),
			spawn(P, process_iq, [Type, IQ, Params, State]),
			ok
	end;

forward_response(_, _, _, _) -> 
	ok.

