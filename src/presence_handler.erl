-module(presence_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/ecomponent.hrl").

%% API
-export([pre_process_presence/3]).

-spec pre_process_presence( undefined | string(), Presence::term(), From::ecomponent:jid()) -> ok.

pre_process_presence(Type, Presence, From) ->
    case Type of
        error ->
            forward_response(#presence{type=Type, from=From, xmlel=Presence}); 
        _ -> 
            forward(#presence{type=Type, from=From, xmlel=Presence})
    end.

-spec forward( Presence::#presence{} ) -> ok.

forward(Presence) ->
    case ecomponent:get_presence_processor() of
        undefined -> 
            spawn(processor, process_presence, [Presence]);
        {mod, P} ->
            spawn(P, process_presence, [Presence]);
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

forward_response(#presence{xmlel=Xmlel}=Presence) ->
    ID = exmpp_stanza:get_id(Xmlel),
    case ecomponent:get_processor(ID) of
        undefined ->
            spawn(processor, process_presence, [Presence]);
        #matching{processor=undefined} ->
            spawn(processor, process_presence, [Presence]);
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
