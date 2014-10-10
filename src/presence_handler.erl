-module(presence_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("ecomponent.hrl").

%% API
-export([pre_process_presence/4]).

-spec pre_process_presence( 
    Type::undefined | string(), 
    Presence::term(), 
    From::ecomponent:jid(),
    ServerID :: atom()) -> ok.
%@doc Pre process the presence stanza. If the presence is an 'error' type 
%     presence should be addressed to the forward_response.
%@end
pre_process_presence("error", Presence, From, ServerID) ->
    To = exmpp_jid:to_lower(exmpp_stanza:get_recipient(Presence)),
    forward_response(#presence{
        type="error", from=From, to=To, xmlel=Presence, server=ServerID});
pre_process_presence(Type, Presence, From, ServerID) ->
    To = exmpp_jid:to_lower(exmpp_stanza:get_recipient(Presence)),
    forward(#presence{
        type=Type, from=From, to=To, xmlel=Presence, server=ServerID}).

-spec forward( Presence::#presence{} ) -> ok.
%@doc Forward the presence directly. This function forward the presence 
%     directly to the application or a process.
%@end
forward(Presence) ->
    case ecomponent:get_presence_processor() of
    undefined -> 
        processor:process_presence(Presence);
    {mod, P} ->
        P:process_presence(Presence);
    {app, Name} ->
        PID = whereis(Name),            
        case erlang:is_pid(PID) andalso erlang:is_process_alive(PID) of
        true -> 
            PID ! Presence;
        _ -> 
            lager:warning("Process not Alive for Presence: ~p~n", [Presence])
        end;
    Proc -> 
        lager:warning("Unknown Request to Forward: ~p ~p~n", [Proc, Presence])
    end.

-spec forward_response( Presence::#presence{} ) -> ok.
%@doc Forward the presence as response. If the type was 'error' this will be
%     forwarded to the correct application or process, or dropped.
%@end
forward_response(#presence{xmlel=Xmlel}=Presence) ->
    ID = exmpp_stanza:get_id(Xmlel),
    case ecomponent:get_processor(ID) of
    undefined ->
        processor:process_presence(Presence);
    #matching{processor=undefined} ->
        processor:process_presence(Presence);
    #matching{processor=App} ->
        PID = whereis(App),
        case is_pid(PID) of 
        true ->
            PID ! Presence;
        _ -> 
            lager:warning("Process not Alive for Presence: ~p~n", [Presence])
        end;
    Proc ->
        lager:warning("Unknown Request to Forward: ~p ~p~n", [Proc, Presence])
    end.
