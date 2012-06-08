-module(default_handler).

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
  process_iq(Type, IQ, From, NS, Payload, State).

process_iq("get", IQ, _, ?NS_PING, _, #state{xmppCom=XmppCom}=State) ->
  Result = exmpp_iq:result(IQ),
  exmpp_component:send_packet(XmppCom, Result),
  {ok, State};

process_iq("error", IQ, _, Ns, State) ->
  choose_processor(IQ, Ns),
  {ok, State};

process_iq("result", IQ, _, Ns, State) ->
  choose_processor(IQ, Ns),
  {ok, State};

process_iq(_, IQ, _, _, _, #state{xmppCom=XmppCom}=State) ->
  lager:info("Unknown Request: ~p~n", [IQ]),
  Error = exmpp_iq:error(IQ, 'service-unavailable'),
  exmpp_component:send_packet(XmppCom, Error),
  {ok, State}.


choose_processor(IQ, Ns) ->
  %%TODO pattern matching namespace choose processor
  lager:info("Choose processor for IQ:~n~p~n", [IQ]);

choose_processor(_, _) ->
  lager:info("Undefined processor").
