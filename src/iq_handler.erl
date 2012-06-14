-module(iq_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/ecomponent.hrl").

%% API
-export([pre_process_iq/4]).

pre_process_iq(Type, IQ, From, PID) ->
	lager:info("Preparing: ~p~n On State:~p~n", [IQ, PID]),
	Payload = exmpp_iq:get_payload(IQ),
	NS = exmpp_xml:get_ns_as_atom(Payload),
	lager:info("NS:~p~n", [NS]),
	process_iq(#params{from=From, ns=NS, type=Type, iq=IQ, payload=Payload}, PID).

process_iq(#params{type="get", iq=IQ, ns=?NS_PING}, PID) ->
	Result = exmpp_iq:result(IQ),
	send(Result, ?NS_PING, PID);

process_iq(#params{type="error"}=Params, PID) ->
	forward_response(Params, PID);

process_iq(#params{type="result"}=Params, PID) ->
        forward_response(Params, PID);

process_iq(#params{type="set"}=Params, PID) ->
        forward_ns(Params, PID);

process_iq(#params{type="get"}=Params, PID) ->
        forward_ns(Params, PID);

process_iq(P, PID) ->
	lager:info("Unknown Request: ~p from ~p~n", [P, PID]).

forward_ns(#params{ns=NS}=Params, ParentPID) ->
	lager:info("Choose processor for IQ:~p~n", [NS]),
	case ecomponent:get_processor_by_ns(NS) of
		undefined -> 
			spawn(?MODULE, handle_unavailable,[Params]);
		{mod, P} ->
			lager:info("Processor ~p ~p", [P, NS]),
			spawn(P, process_iq, [Params, ParentPID]);
		{app, Name} ->
			PID = whereis(Name),			
			case erlang:is_pid(PID) andalso erlang:is_process_alive(PID) of
				true -> 
					PID ! {iq, Params, ParentPID};
				_ -> 
					lager:warning("Process not Alive for NS: ~p~n", [NS])
			end;
		Proc -> 
			lager:warning("Unknown Request to Forward: ~p ~p~n", [Proc, Params])
	end.

forward_response(#params{iq=#iq{id=ID}}=Params, PID) ->
	case ecomponent:get_processor(ID) of
		#matching{ns=NS, processor=PID} when is_pid(PID) ->
			lager:info("Processor ~p ~p ~n", [PID, NS]),
			PID ! {response, NS, Params},
			ok;
		_ -> 
			ok
	end;

forward_response(_, _) -> 
	ok.

send(Packet, NS, PID) when is_pid(PID) ->
	PID ! {send, Packet, NS, PID};
send(_, _, PID) -> 
	lager:warn("Invalid PID to send packet ~p~n", [PID]),
	ok.
