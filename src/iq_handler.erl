-module(iq_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/ecomponent.hrl").

-record(params, {from, to, ns, payload}).

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
	ecomponent:send_packet(XmppCom, Result),
	{ok, State};

process_iq("error"=Type, IQ, #params{from=_From, ns=_Ns, payload=_Payload}=Params, State) ->
	forward_response(Type, IQ, Params),
	{ok, State};

process_iq("result"=Type, IQ, #params{}=Params, State) ->
	forward_response(Type, IQ, Params),
	{ok, State};

process_iq("set"=Type, IQ, #params{}=Params, #state{xmppCom=XmppCom}=State) ->
	forward_ns(Type, IQ, Params),
	{ok, State};

process_iq("get"=Type, IQ, #params{}=Params, #state{xmppCom=XmppCom}=State) ->
	forward_ns(Type, IQ, Params),
	{ok, State};

process_iq(_, IQ, _, State) ->
	lager:info("Unknown Request: ~p~n", [IQ]),
	{ok, State}.

forward_ns(Type, IQ, #params{}=Params) ->
	%%TODO pattern matching namespace choose processor
	lager:info("Choose processor for IQ:~p~n", [IQ]).

forward_response(_, _, _) -> 
	ok.
