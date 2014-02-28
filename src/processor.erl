%@doc The processor module is used when a processor is not found.
%     This module is used when a processor is not found. The stanza
%     is sent to this module and the module report to the logs or
%     reply to the stanza with a 'feature-not-implemented'.
%@end
-module(processor).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("ecomponent.hrl").

%% API
-export([process_iq/1, process_message/1, process_presence/1]).

-spec process_iq( Params::#params{} ) -> ok.
%@doc Process the IQ stanza. Returns a feature-not-implemented error.
%@end
process_iq(#params{iq=IQ}) ->
    lager:info("Unknown Request: ~p~n", [IQ]),
    Error = exmpp_iq:error(IQ, 'feature-not-implemented'),
    ecomponent:send(Error, ecomponent),
    ok.

-spec process_message( Message::#message{} ) -> ok.
%@doc Process the message stanza. Drops the message.
process_message(#message{}=Message) ->
    lager:info("Message received: ~p", [Message]).

-spec process_presence( Presence::#presence{} ) -> ok.
%@doc Process the presence stanza. Drops the message.
process_presence(#presence{}=Presence) ->
    lager:info("Presence received: ~p", [Presence]).
