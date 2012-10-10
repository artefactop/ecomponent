%%%-------------------------------------------------------------------
%%% File        : ecomponent.erl
%%% Author      : Jose Luis Navarro <pepe@yuilop.com>
%%%               Manuel Rubio <manuel@yuilop.com>
%%% Description : ecomponent Service - External Component
%%%-------------------------------------------------------------------
-module(ecomponent).

-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/ecomponent.hrl").

-type jid() :: { Name::string(), Server::string(), Resource::string() }.

-type mod_processor() :: {mod, Module::atom()}.

-type app_processor() :: {app, App::atom()}.

-type processor() :: {Name::atom(), Value::(mod_processor() | app_processor())}.

-type accesslist() :: Domains::list(binary()).

-define(MAX_PER_PERIOD, 10).
-define(PERIOD_SECONDS, 6).
-define(MAX_TRIES, 3).
-define(RESEND_PERIOD, 100).
-define(REQUEST_TIMEOUT, 10).
-define(SYSLOG_FACILITY, local7).
-define(SYSLOG_NAME, "ecomponent").
-define(DEFAULT_ID, <<"ec_00000000">>).

-record(state, {
    xmppCom :: pid(),
    jid :: jid(),
    id = ?DEFAULT_ID :: string(),
    pass :: string(),
    server :: string(),
    port :: integer(),
    whiteList :: list(string()),
    maxPerPeriod = ?MAX_PER_PERIOD :: integer(),
    periodSeconds = ?PERIOD_SECONDS :: integer(),
    processors :: list(processor()),
    maxTries = ?MAX_TRIES :: integer(),
    resendPeriod = ?RESEND_PERIOD :: integer(),
    requestTimeout = ?REQUEST_TIMEOUT :: integer(),
    accessListSet = [] :: accesslist(),
    accessListGet = [] :: accesslist(),
    syslogFacility = ?SYSLOG_FACILITY :: atom(),
    syslogName = ?SYSLOG_NAME :: string()
}).

%% API
-export([prepare_id/1, unprepare_id/1, get_processor/1, get_processor_by_ns/1, send/3, send/2, save_id/4, cleanup_expired/1, syslog/2, configure/0]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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

-spec init( Args :: [] ) -> 
    {ok, State :: #state{}} | 
    {ok, State :: #state{}, Timeout :: integer()} |
    ignore | {stop, Reason :: string()}.

init([]) ->
    lager:info("Loading Application eComponent", []),
    timem:init(),
    configure().

-spec configure() -> #state{}.

configure() ->
    Conf = confetti:fetch(ecomponent_conf),
    Facility = proplists:get_value(syslog_facility, Conf, local7),
    Name = proplists:get_value(syslog_name, Conf, "ecomponent"),
    init_syslog(Facility, Name),
    [ lager:info("~p = ~p", [X,Y]) || {X,Y} <- Conf ],

    JID = proplists:get_value(jid, Conf),
    Pass = proplists:get_value(pass, Conf),
    Server = proplists:get_value(server, Conf),
    Port = proplists:get_value(port, Conf),
    WhiteList = proplists:get_value(whitelist, Conf, []),
    Processors = proplists:get_value(processors, Conf, []),

    mod_monitor:init(WhiteList),
    prepare_processors(Processors),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    {ok, #state{
        xmppCom = XmppCom,
        jid = JID,
        pass = Pass,
        server = Server,
        port = Port,
        whiteList = WhiteList,
        maxPerPeriod = proplists:get_value(max_per_period, Conf, ?MAX_PER_PERIOD),
        periodSeconds = proplists:get_value(period_seconds, Conf, ?PERIOD_SECONDS),
        processors = proplists:get_value(processors, Conf),
        maxTries = proplists:get_value(max_tries, Conf, ?MAX_TRIES),
        resendPeriod = proplists:get_value(resend_period, Conf, ?RESEND_PERIOD),
        requestTimeout = proplists:get_value(request_timeout, Conf, ?REQUEST_TIMEOUT),
        accessListSet = proplists:get_value(access_list_set, Conf, []),
        accessListGet = proplists:get_value(access_list_get, Conf, [])
    }}.

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

handle_info({send, OPacket, NS, App}, #state{jid=JID, xmppCom=XmppCom, id=ID}=State) ->
    Kind = exmpp_iq:get_kind(OPacket),
    From = exmpp_stanza:get_sender(OPacket),
    NewPacket = case From of
        undefined ->
            exmpp_xml:set_attribute(OPacket, <<"from">>, JID);
        _ ->
            OPacket
    end,
    Packet = case Kind of
        request ->
            P = exmpp_xml:set_attribute(NewPacket, <<"id">>, ID),
            save_id(ID, NS, P, App),
            P;
        _ -> 
            NewPacket
    end,
    lager:debug("Sending packet ~p",[Packet]),
    exmpp_component:send_packet(XmppCom, Packet),
    {noreply, State#state{id=gen_id(ID)}};

handle_info({resend, #matching{tries=Tries, packet=P}=N}, #state{xmppCom = XmppCom, maxTries=Max}=State) when Tries < Max ->
    save_id(N#matching{tries=Tries+1}),
    exmpp_component:send_packet(XmppCom, P),
    {noreply, State};

handle_info({resend, #matching{tries=Tries}=N}, #state{maxTries=Max}=State) when Tries >= Max ->
    lager:warning("Max tries exceeded for: ~p~n", [N]),
    {noreply, State};

handle_info({_, tcp_closed}, #state{jid=JID, server=Server, pass=Pass, port=Port}=State) ->
    lager:info("Connection Closed. Trying to Reconnect...~n", []),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    lager:info("Reconnected.~n", []),
    {noreply, State#state{xmppCom=XmppCom}};

handle_info({_,{bad_return_value, _}}, #state{jid=JID, server=Server, pass=Pass, port=Port}=State) ->
    lager:info("Connection Closed. Trying to Reconnect...~n", []),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    lager:info("Reconnected.~n", []),
    {noreply, State#state{xmppCom=XmppCom}};

handle_info(stop, #state{xmppCom=XmppCom}=State) ->
    lager:info("Component Stopped.~n",[]),
    exmpp_component:stop(XmppCom),
    {stop, normal, State};

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
handle_call({access_list_set, NS, Jid} = Info, _From, State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, is_allowed(set, NS, Jid, State), State};

handle_call({access_list_get, NS, Jid} = Info, _From, State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, is_allowed(get, NS, Jid, State), State};

handle_call(reconnect, _From, State) ->
    exmpp_component:stop(State#state.xmppCom),
    timer:sleep(250),
    {_, XmppCom} = make_connection(
        State#state.jid, State#state.pass,
        State#state.server, State#state.port
    ),
    {reply, ok, State#state{xmppCom=XmppCom}};

handle_call({change_config, syslog, {Facility, Name}}, _From, State) ->
    init_syslog(if
        is_number(Facility) -> Facility;
        true -> list_to_atom(Facility)
    end, Name),
    {reply, ok, State};

handle_call({change_config, Key, Value}, _From, State) ->
    case Key of
        maxPerPeriod -> {reply, ok, State#state{maxPerPeriod=Value}};
        periodSeconds -> {reply, ok, State#state{periodSeconds=Value}};
        maxTries -> {reply, ok, State#state{maxTries=Value}};
        resendPeriod -> {reply, ok, State#state{resendPeriod=Value}};
        requestTimeout -> {reply, ok, State#state{requestTimeout=Value}};
        _ -> {reply, error, State}
    end;

handle_call({set_xmpp_conf, JID, Pass, Server, Port}, _From, State) ->
    exmpp_component:stop(State#state.xmppCom),
    timer:sleep(250),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    {reply, ok, State#state{jid=JID, pass=Pass, server=Server, port=Port, xmppCom=XmppCom}};

handle_call(get_xmpp_conf, _From, State) ->
    {reply, [State#state.jid, State#state.pass, State#state.server, State#state.port], State};
        
handle_call(Info, _From, _State) ->
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

increment(A) when 
    (A >= $0 andalso A < $9) orelse
    (A >= $A andalso A < $Z) orelse
    (A >= $a andalso A < $z) ->
    A+1;
increment($9) ->
    $A;
increment($Z) ->
    $a.

gen_id(B) ->
    gen_id(lists:reverse(binary_to_list(B)), [], true).

gen_id([], Result, _) ->
    list_to_binary(Result);
gen_id([$_|T], Result, true) ->
    gen_id(T, [$_|Result], false);
gen_id([$z|T], Result, true) ->
    gen_id(T, [$0|Result], true);
gen_id([H|T], Result, true) ->
    gen_id(T, [increment(H)|Result], false);
gen_id([H|T], Result, false) ->
    gen_id(T, [H|Result], false).

init_syslog(Facility, Name) ->
    lager:info("Syslog configured: facility=~p, name=~p",[Facility, Name]),
    syslog:open(Name, [cons, perror, pid], Facility).

save_id(Id, NS, Packet, App) ->
    N = #matching{id=Id, ns=NS, processor=App, tries=0, packet=Packet},
    save_id(N).
    
save_id(#matching{id=Id, processor=App}=N) ->
    case timem:insert(Id, N) of
        true ->
            N;
        _ ->
            lager:error("Error writing id ~s, processor ~p on timem, reason: ~p", [Id, App])
    end.

cleanup_expired(D) ->
    L = timem:remove_expired(D),
    [ resend(N) || {_K, N} <- L, is_record(N, matching) ],
    ok.

resend(N) when is_record(N, matching) ->
    ?MODULE ! {resend, N},
    ok.

get_processor(Id) ->
    case timem:remove(Id) of
        {_, N} when is_record(N, matching) -> 
            N;
        _ ->
            lager:warning("Found no matching processor for id ~s",[Id]),
            undefined
    end.

prepare_processors(P) ->
    case ets:info(?NS_PROCESSOR) of
        undefined ->
            ets:new(?NS_PROCESSOR, [named_table, public]);
        _ ->
            ets:delete_all_objects(?NS_PROCESSOR)
    end,
    [ 
        ets:insert(?NS_PROCESSOR, {NS, {Type, Processor}}) ||
        {NS, {Type, Processor}} <- P
    ],
    ok.

get_processor_by_ns(NS) ->
    case ets:lookup(?NS_PROCESSOR, NS) of
        [{_, {_T, _P}=Result}] -> Result;
        _ ->
            case ets:lookup(?NS_PROCESSOR, default) of
                [{_, {_T, _P}=Result}] -> Result;
                _ -> []
            end
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
        R -> 
            exmpp_component:handshake(XmppCom),
            lager:info("Connected.~n",[]),
            {R, XmppCom}
    catch
        Exception ->
            lager:warning("Exception: ~p~n",[Exception]),
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

send(Packet, App) ->
    Payload = exmpp_iq:get_payload(Packet),
    NS = exmpp_xml:get_ns_as_atom(Payload),
    send(Packet, NS, App).

send(Packet, NS, App) ->
    ?MODULE ! {send, Packet, NS, App},
    ok.

is_allowed(set, NS, {_, Domain, _}, #state{accessListSet=As}) ->
    is_allowed(NS, Domain, As);
is_allowed(get, NS, {_, Domain, _}, #state{accessListGet=Ag}) ->
    is_allowed(NS, Domain, Ag).

is_allowed(NS, Domain, PList) ->
    case proplists:get_value(NS, PList) of
        undefined ->
            true;
        List ->
            lists:member(Domain, List)
    end.

%% Level: emerg, alert, crit, err, warning, notice, info, debug
syslog(Level, Message) when is_binary(Message) ->
    syslog(Level, erlang:binary_to_list(Message));
syslog(Level, Message) when is_list(Message) ->
    Priority = case Level of
        emerg -> "EMERG ";
        alert -> "ALERT ";
        crit -> "CRIT ";
        err -> "ERR ";
        warning -> "WARNING ";
        notice -> "NOTICE ";
        info -> "INFO ";
        debug -> "DEBUG ";
        _ -> ""
    end,
    syslog:log(Level, Priority ++ Message).

