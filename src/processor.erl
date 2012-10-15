-module(processor).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/ecomponent.hrl").

%% API
-export([process_iq/4, process_message/1]).

process_iq(#params{type="error", iq=IQ}) ->
    lager:info("Get error IQ: ~p", [IQ]),
    ok;

process_iq(#params{type="result", iq=IQ}) ->
    lager:info("Get result IQ: ~p", [IQ]),
    ok;

process_iq(#params{iq=IQ}) ->
    lager:info("Unknown Request: ~p~n", [IQ]),
    Error = exmpp_iq:error(IQ, 'service-unavailable'),
    ecomponent:send_packet(Error),
    ok.

process_message(#message{}=Message) ->
	lager:info("Message received: ~p", [Message]).
