%%%-------------------------------------------------------------------
%%% File		: ecomponent.erl
%%% Author	: Jose Luis Navarro <pepe@yuilop.com>
%%% Description : ecomponent Service - External Component
%%% Provides:
%%%		
%%%
%%% Created : 07 Jun 2012 by Jose Luis Navarro <pepe@yuilop.com>
%%%-------------------------------------------------------------------

-module(ecomponent).
-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/ecomponent.hrl").

%% API
-export([prepare_id/1, unprepare_id/1, is_allowed/2, get_processor/1, get_processor_by_ns/1, send/3, send/2, send/1]).

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
	mnesia:delete_table(matching),
	mnesia:create_table(matching, [{attributes, record_info(fields, matching)}]),
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
	lager:info("mod_monitor started"),
	{_, XmppCom} = make_connection(JID, Pass, Server, Port),
	{ok, #state{xmppCom=XmppCom, jid=JID, pass=Pass, server=Server, port=Port, whiteList=WhiteList, maxPerPeriod=MaxPerPeriod, periodSeconds=PeriodSeconds, processors=Processors}};
init(_, _, _, _, _, _, _ , _) ->
lager:error("Some param is undefined"),
	{error, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(#received_packet{packet_type=iq, type_attr=Type, raw_packet=IQ, from=From}, #state{maxPerPeriod=MaxPerPeriod, periodSeconds=PeriodSeconds}=State) ->
	case mod_monitor:accept(From, MaxPerPeriod, PeriodSeconds) of
		true ->
			spawn(iq_handler, pre_process_iq, [Type, IQ, From, whereis(?MODULE)]),
			{noreply, State};
		_ ->
			{noreply, State}
	end;

handle_info({send, Packet, NS, PID}, #state{xmppCom=XmppCom}=State) ->
	Kind = exmpp_iq:get_kind(Packet),
	ID = exmpp_stanza:get_id(Packet),
	case Kind of
		request -> 
			save_id(ID, NS, PID);
		_ -> 
			ok
	end,
        exmpp_component:send_packet(XmppCom, Packet),
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
	lager:info("Terminating Component...", []),
	application:stop(exmpp),
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

save_id(Id, NS, Processor) ->
	N = #matching{id=Id, ns=NS, processor=Processor},
	case mnesia:write({matching, N}) of
	{'EXIT', Reason} ->
		lager:error("Error writing id ~s, processor ~p on mnesia, reason: ~p", [Id, Processor, Reason]);
	_ -> N
	end.

get_processor(Id) ->
	V = mnesia:dirty_read(matching, Id),
	case V of
	{'EXIT', Reason} ->
		lager:error("Error getting processor with id ~s on mnesia, reason: ~p",[Id, Reason]),
		undefined; 
	[] -> 
		lager:warning("Found no processor for ~s",[Id]),
		undefined;
	[#matching{}=N|_] -> 
		N;
	_ ->
		lager:warning("Found no matching processor for ~s",[Id])
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
	lager:info("Search namespace for ~p~n", [NS]),
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
		Exception -> lager:warning("Exception: ~p~n",[Exception]),	 %%TODO change for lager
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

send(Packet) ->
	Payload = exmpp_iq:get_payload(Packet),
	NS = exmpp_xml:get_ns_as_atom(Payload),
	send(Packet, NS).

send(Packet, NS) ->
        send(Packet, NS, whereis(?MODULE)).

send(Packet, NS, PID) when is_pid(PID) ->
        PID ! {send, Packet, NS, PID};
send(_, _, PID) ->
        lager:warn("Invalid PID to send packet ~p~n", [PID]),
        ok.

