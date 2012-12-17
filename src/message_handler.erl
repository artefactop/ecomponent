-module(message_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/ecomponent.hrl").

%% API
-export([pre_process_message/3]).

-spec pre_process_message( undefined | string(), Message::term(), From::ecomponent:jid()) -> ok.

pre_process_message(Type, Message, From) ->
    case Type of
        undefined ->
            forward(#message{type=normal, from=From, xmlel=Message});
        error ->
            forward_response(#message{type=Type, from=From, xmlel=Message}); 
        _ -> 
            forward(#message{type=Type, from=From, xmlel=Message})
    end.

-spec forward( Message::#message{} ) -> ok.

forward(Message) ->
    case ecomponent:get_message_processor() of
        undefined -> 
            spawn(processor, process_message, [Message]);
        {mod, P} ->
            spawn(P, process_message, [Message]);
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

forward_response(#message{xmlel=Xmlel}=Message) ->
    ID = exmpp_stanza:get_id(Xmlel),
    case ecomponent:get_processor(ID) of
        undefined ->
            spawn(processor, process_message, [Message]);
        #matching{processor=undefined} ->
            spawn(processor, process_message, [Message]);
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
