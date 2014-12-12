-module(message_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("ecomponent.hrl").

%% API
-export([pre_process_message/4]).

-spec pre_process_message( 
    Type::undefined | string(), 
    Message::term(), 
    From::ecomponent:jid(),
    ServerID::atom()) -> ok.
%@doc Pre process the message stanza. If the message is an 'error' type 
%     message should be addressed to the forward_response.
%@end
pre_process_message(undefined, Message, From, ServerID) ->
    forward(#message{
        type="normal", from=From, xmlel=Message, server=ServerID});
pre_process_message("error", Message, From, ServerID) ->
    To = exmpp_jid:to_lower(exmpp_stanza:get_recipient(Message)),
    forward_response(#message{
        type="error", from=From, to=To, xmlel=Message, server=ServerID});
pre_process_message(Type, Message, From, ServerID) ->
    To = exmpp_jid:to_lower(exmpp_stanza:get_recipient(Message)),
    forward(#message{
        type=Type, from=From, to=To, xmlel=Message, server=ServerID}).

-spec forward( Message::#message{} ) -> ok.
%@doc Forward the message directly. This function forward the message 
%     directly to the application or a process.
%@end
forward(Message) ->
    case ecomponent:get_message_processor() of
        undefined -> 
            processor:process_message(Message);
        {mod, P} ->
            P:process_message(Message);
        {app, Name} ->
            PID = whereis(Name),            
            case erlang:is_pid(PID) andalso erlang:is_process_alive(PID) of
                true -> 
                    PID ! Message;
                _ -> 
                    lager:warning("Process not Alive for Message: ~p~n", [Message])
            end;
        Proc -> 
            lager:warning("Unknown Request to Forward: ~p ~p~n", [Proc, Message])
    end.

-spec forward_response( Message::#message{} ) -> ok.
%@doc Forward the message as response. If the type was 'error' this will be
%     forwarded to the correct application or process, or dropped.
%@end
forward_response(#message{xmlel=Xmlel}=Message) ->
    ID = exmpp_stanza:get_id(Xmlel),
    case ecomponent:get_processor(ID) of
        undefined ->
            processor:process_message(Message);
        #matching{processor=undefined} ->
            processor:process_message(Message);
        #matching{processor=App} ->
            PID = whereis(App),
            case is_pid(PID) of 
                true ->
                    PID ! Message;
                _ -> 
                    lager:warning("Process not Alive for Message: ~p~n", [Message])
            end;
        Proc ->
            lager:warning("Unknown Request to Forward: ~p ~p~n", [Proc, Message])
    end.
