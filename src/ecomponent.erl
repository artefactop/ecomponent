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
-export([get_stats/0, prepare_id/1, unprepare_id/1, is_allowed/2, get_port/1, send_packet/2]).

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
	init(application:get_env(ecomponent, jid),
			 application:get_env(ecomponent, pass),
			 application:get_env(ecomponent, server),
			 application:get_env(ecomponent, port),
			 application:get_env(ecomponent, whitelist),
			 application:get_env(ecomponent, max_per_period),
			 application:get_env(ecomponent, period_seconds),
			 application:get_env(ecomponent, handler)).


init(JID, Pass, Server, Port, WhiteList, MaxPerPeriod, PeriodSeconds, Handler) ->
		application:start(exmpp),
		mod_monitor:init(WhiteList),
		{_, XmppCom} = make_connection(JID, Pass, Server, Port),
		{ok, #state{xmppCom=XmppCom, jid=JID, pass=Pass, server=Server, port=Port, whiteList=WhiteList, maxPerPeriod=MaxPerPeriod, periodSeconds=PeriodSeconds, handler=Handler}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(#received_packet{packet_type=iq, type_attr=Type, raw_packet=IQ, from=From}, #state{maxPerPeriod=MaxPerPeriod, periodSeconds=PeriodSeconds, handler=Handler}=State) ->
	case mod_monitor:accept(From, MaxPerPeriod, PeriodSeconds) of
		true ->
			spawn(Handler, pre_process_iq, [Type, IQ, From, State]),
			{noreply, State};
		_ ->
			{noreply, State}
	end;

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

send_packet(XmppCom, Packet) ->
	exmpp_component:send_packet(XmppCom, Packet). %%TODO connection

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
		Exception -> lager:warn("Exception: ~p~n",[Exception]),	 %%TODO change for lager
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

get_port(PortMonitor) -> get_port(PortMonitor, 5).
get_port(_, 0) -> 
	lager:error("Problem Retrieving Port Number",[]),
	{error, "Problem Retrieving Port Number"};
get_port(PortMonitor, T) ->
	PortMonitor ! {get_port, self()},
	receive
		{ok, Port} -> {ok, Port};
		{error, R} -> {error, R}
	after 200 -> get_port(PortMonitor, T-1) 
	end.

get_stats() ->
	get_stats(3, whereis(ecomponent)).
get_stats(0, _) -> -1;
get_stats(N, PID) ->
	PID!{get_active, self()},
	receive 
		{result_active, A} -> A
	after 100 -> get_stats(N-1, PID)
	end.
