%%%-------------------------------------------------------------------
%%% File		: ecomponent.erl
%%% Author	: Jose Luis Navarro <pepe@yuilop.com>
%%% Description : ecomponent Service - External Component
%%% Provides:
%%%		
%%elf

%%% Created : 07 Jun 2012 by Jose Luis Navarro <pepe@yuilop.com>
%%%-------------------------------------------------------------------

-module(ecomponent).
-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/ecomponent.hrl").

%% API
-export([prepare_id/1, unprepare_id/1, is_allowed/2, get_processor/1, get_processor_by_ns/1, send/3, send/2, save_id/4, cleanup_expired/1]).

%% gen_server callbacks
-export([start_link/0, init/8, init/1, handle_call/3, handle_cast/2, handle_info/2,
				 terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_) ->
	lager:info("Loading Application eComponent", []),
	timem:init(),
	init(application:get_env(ecomponent, jid),
			 application:get_env(ecomponent, pass),
			 application:get_env(ecomponent, server),
			 application:get_env(ecomponent, port),
			 application:get_env(ecomponent, whitelist),
			 application:get_env(ecomponent, max_per_period),
			 application:get_env(ecomponent, period_seconds),
			 application:get_env(ecomponent, processors)).

init({_,JID}, {_,Pass}, {_,Server}, {_,Port}, {_,WhiteList}, {_,MaxPerPeriod}, {_,PeriodSeconds}, {_,Processors}) ->
	lager:info("JID ~p", [JID]),
	lager:info("Pass ~p", [Pass]),
	lager:info("Server ~p", [Server]),
	lager:info("Port ~p", [Port]),
	lager:info("WhiteList ~p", [WhiteList]),
	lager:info("MaxPerPeriod ~p", [MaxPerPeriod]),
	lager:info("PeriodSeconds ~p", [PeriodSeconds]),
	lager:info("Processors ~p", [Processors]),
	mod_monitor:init(WhiteList),
	prepare_processors(Processors),
	{_, XmppCom} = make_connection(JID, Pass, Server, Port),
	{ok, #state{xmppCom=XmppCom, jid=JID, iqId = 1, pass=Pass, server=Server, port=Port, whiteList=WhiteList, maxPerPeriod=MaxPerPeriod, periodSeconds=PeriodSeconds, processors=Processors, maxTries=3, resendPeriod=100, requestTimeout=10}};
init(_, _, _, _, _, _, _ , _) ->
	lager:error("Some param is undefined"),
	{error, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(#received_packet{packet_type=iq, type_attr=Type, raw_packet=IQ, from={Node, Domain, _}=From}, #state{maxPerPeriod=MaxPerPeriod, periodSeconds=PeriodSeconds}=State) ->
	case mod_monitor:accept(exmpp_jid:to_list(Node, Domain), MaxPerPeriod, PeriodSeconds) of
		true ->
			spawn(iq_handler, pre_process_iq, [Type, IQ, From]),
			{noreply, State};
		_ ->
			{noreply, State}
	end;

handle_info({send, OPacket, NS, App}, #state{jid=JID, xmppCom=XmppCom, iqId=IqID, resendPeriod=RP, requestTimeout=RT}=State) ->
	Kind = exmpp_iq:get_kind(OPacket),
	From = exmpp_stanza:get_sender(OPacket),
        case From of
                undefined ->
                        NewPacket = exmpp_xml:set_attribute(OPacket, <<"from">>, JID);
                _ ->
			NewPacket = OPacket
        end,
	case Kind of
		request -> 
			Packet = exmpp_xml:set_attribute(NewPacket, <<"id">>, erlang:integer_to_list(IqID)),
		        ID = exmpp_stanza:get_id(Packet),
			save_id(ID, NS, Packet, App);
		_ -> 
			Packet = NewPacket
	end,
    	exmpp_component:send_packet(XmppCom, Packet),
	case IqID rem RP of
		0 ->
			cleanup_expired(RT);
		_ -> 
			ok
	end,
	{noreply, State#state{iqId=IqID+1}};

handle_info({resend, #matching{tries=Tries, packet=P}=N}, #state{xmppCom = XmppCom, maxTries=Max}=State) ->
	case Tries < Max of
		true ->
			save_id(N#matching{tries=Tries+1}),
			exmpp_component:send_packet(XmppCom, P);
		_ ->
			lager:warning("Max tries exceeded for: ~p~n", [N])
	end,
	{noreply, State};

handle_info({_, tcp_closed}, #state{jid=JID, server=Server, pass=Pass, port=Port}=State) ->
	lager:info("Connection Closed. Trying to Reconnect...~n", []),
	{_, NewXmppCom} = make_connection(JID, Pass, Server, Port),
	lager:info("Reconnected.~n", []),
	{noreply, State#state{xmppCom=NewXmppCom}};

handle_info({_,{bad_return_value, _}}, #state{jid=JID, server=Server, pass=Pass, port=Port}=State) ->
	lager:info("Connection Closed. Trying to Reconnect...~n", []),
	{_, NewXmppCom} = make_connection(JID, Pass, Server, Port),
	lager:info("Reconnected.~n", []),
	{noreply, State#state{xmppCom=NewXmppCom}};

handle_info(stop, #state{xmppCom=XmppCom}=State) ->
	lager:info("Component Stopped.~n",[]),
	exmpp_component:stop(XmppCom),
	{noreply, State};

handle_info(Record, State) -> 
	lager:info("Unknown Info Request: ~p~n", [Record]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	lager:info("Received: ~p~n", [_Msg]), 
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Info,_From, _State) ->
	lager:info("Received Call: ~p~n", [Info]),
	{reply, ok, _State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	lager:info("Terminated Component.", []),
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
save_id(_, _, _, undefined) -> ok;
save_id(Id, NS, Packet, App) ->
	N = #matching{id=Id, ns=NS, processor=App, tries=1, packet=Packet},
	save_id(N).
	
save_id(#matching{id=Id, processor=App}=N) ->
	case timem:insert(Id, N) of
		true ->
			N;
		_ ->
                lager:error("Error writing id ~s, processor ~p on timem, reason: ~p", [Id, App])
	end;
save_id(_M) -> 
	lager:warning("Not Match found for saving id: ~p~n", [_M]).

cleanup_expired(D) ->
	L = timem:remove_expired(D),
	cleanup_expired(D, L).

cleanup_expired(_D, []) ->
	ok;
cleanup_expired(D, [{_K, #matching{}=N }|T]) ->
	resend(N),
	cleanup_expired(D, T).
	
resend(#matching{}=N) ->
        case whereis(?MODULE) of
                undefined ->
                        ok;
                MPID when is_pid(MPID) ->
                        MPID ! {resend, N}
        end.

get_processor(Id) ->
	V = timem:remove(Id),
	case V of
		{_, #matching{}=N} -> 
			N;
		_ ->
			lager:warning("Found no matching processor for ~s",[Id]),
			undefined
	end.

prepare_processors(P) ->
    case ets:info(?NS_PROCESSOR) of
            undefined ->
                    ets:new(?NS_PROCESSOR, [named_table, public]);
            _ ->
                    ets:delete_all_objects(?NS_PROCESSOR)
    end,
    p_p(P).

p_p([]) -> ok;
p_p([{NS, {Type, Processor}}|T]) ->
        ets:insert(?NS_PROCESSOR, {NS, {Type, Processor}}),
        p_p(T);
p_p(_P) ->
        lager:warning("Unexpected NS/Processor Pair ~p~n",[_P]),
        ok.

get_processor_by_ns(NS) ->
	 case ets:lookup(?NS_PROCESSOR, NS) of
                [{_, {_T, _P}=Result}] -> Result;
                _ -> []
        end.

make_connection(JID, Pass, Server, Port) -> 
	XmppCom = exmpp_component:start(),
	make_connection(XmppCom, JID, Pass, Server, Port, 20).
make_connection(XmppCom, JID, Pass, Server, Port, 0) -> 
	exmpp_component:stop(XmppCom),
	make_connection(JID, Pass, Server, Port);
make_connection(XmppCom, JID, Pass, Server, Port, Tries) ->
	lager:info("Connecting: ~p Tries Left~n",[Tries]),
	exmpp_component:auth(XmppCom, JID, Pass),
	try exmpp_component:connect(XmppCom, Server, Port) of
		R -> exmpp_component:handshake(XmppCom),
		lager:info("Connected.~n",[]),
		{R, XmppCom}
	catch
		Exception -> lager:warning("Exception: ~p~n",[Exception]),
		timer:sleep((20-Tries) * 200),
		make_connection(XmppCom, JID, Pass, Server, Port, Tries-1)
	end.

prepare_id([]) -> [];
prepare_id([$<|T]) -> [$x|prepare_id(T)];
prepare_id([$>|T]) -> [$X|prepare_id(T)];
prepare_id([H|T]) -> [H|prepare_id(T)].

unprepare_id([]) -> [];
unprepare_id([$x|T]) -> [$<|unprepare_id(T)];
unprepare_id([$X|T]) -> [$>|unprepare_id(T)];
unprepare_id([H|T]) -> [H|unprepare_id(T)].

is_allowed(_, []) -> true;
is_allowed({_,D,_}, WhiteDomain) ->
	is_allowed(D, WhiteDomain);
is_allowed(Domain, WhiteDomain) -> 
	lists:any(fun(S) -> S == Domain end, WhiteDomain).

send(Packet, App) ->
	Payload = exmpp_iq:get_payload(Packet),
	NS = exmpp_xml:get_ns_as_atom(Payload),
	send(Packet, NS, App).

send(Packet, NS, App) ->
	case whereis(?MODULE) of
		undefined -> 
			ok;
		MPID when is_pid(MPID) -> 
		        MPID ! {send, Packet, NS, App}
	end.
