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

-record(state, {
    xmppCom :: pid(),
    jid :: jid(),
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
    syslogName = ?SYSLOG_NAME :: string(),
    timeout = undefined :: integer()
}).

%% API
-export([prepare_id/1, unprepare_id/1, get_processor/1, get_processor_by_ns/1, send/3, send/2, save_id/4, syslog/2, configure/0, gen_id/0, reset_countdown/1, get_countdown/1]).

%% gen_server callbacks
-export([start_link/0, stop/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec start_link() -> {ok, Pid::pid()} | {error, Reason::any()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.

stop() ->
    gen_server:call(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init( Args :: [] ) -> 
    {ok, State :: #state{}} | 
    {ok, State :: #state{}, hibernate | infinity | non_neg_integer()} |
    ignore | {stop, Reason :: string()}.

init([]) ->
    lager:info("Loading Application eComponent", []),
    timem:init(),
    configure().

-spec handle_info(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), State::#state{}}.

handle_info(#received_packet{packet_type=iq, type_attr=Type, raw_packet=IQ, from={Node, Domain, _}=From}, #state{maxPerPeriod=MaxPerPeriod, periodSeconds=PeriodSeconds}=State) ->
    case mod_monitor:accept(exmpp_jid:to_list(Node, Domain), MaxPerPeriod, PeriodSeconds) of
        true ->
            spawn(iq_handler, pre_process_iq, [Type, IQ, From]),
            {noreply, State, get_countdown(State)};
        false ->
            {noreply, State, get_countdown(State)}
    end;

handle_info({send, OPacket, NS, App}, #state{jid=JID, xmppCom=XmppCom}=State) ->
    ID = gen_id(),
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
    {noreply, State, get_countdown(State)};

handle_info({resend, #matching{tries=Tries, packet=P}=N}, #state{xmppCom = XmppCom, maxTries=Max}=State) when Tries < Max ->
    save_id(N#matching{tries=Tries+1}),
    exmpp_component:send_packet(XmppCom, P),
    {noreply, State, get_countdown(State)};

handle_info({resend, #matching{tries=Tries}=N}, #state{maxTries=Max}=State) when Tries >= Max ->
    lager:warning("Max tries exceeded for: ~p~n", [N]),
    {noreply, State, get_countdown(State)};

handle_info({_, tcp_closed}, #state{jid=JID, server=Server, pass=Pass, port=Port}=State) ->
    lager:info("Connection Closed. Trying to Reconnect...~n", []),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    lager:info("Reconnected.~n", []),
    {noreply, State#state{xmppCom=XmppCom}, get_countdown(State)};

handle_info({_,{bad_return_value, _}}, #state{jid=JID, server=Server, pass=Pass, port=Port}=State) ->
    lager:info("Connection Closed. Trying to Reconnect...~n", []),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    lager:info("Reconnected.~n", []),
    {noreply, State#state{xmppCom=XmppCom}, get_countdown(State)};

handle_info(stop, #state{xmppCom=XmppCom}=State) ->
    lager:info("Component Stopped.~n",[]),
    exmpp_component:stop(XmppCom),
    {stop, normal, State};

handle_info(timeout, #state{requestTimeout=RT}=State) ->
    expired_stanzas(RT),
    {noreply, reset_countdown(State), State#state.requestTimeout};

handle_info(Record, State) -> 
    lager:info("Unknown Info Request: ~p~n", [Record]),
    {noreply, State, get_countdown(State)}.


-spec handle_cast(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), State::#state{}}.

handle_cast(_Msg, State) ->
    lager:info("Received: ~p~n", [_Msg]), 
    {noreply, State, get_countdown(State)}.


-spec handle_call(Msg::any(), From::{pid(),_}, State::#state{}) ->
    {reply, Reply::any(), State::#state{}} |
    {reply, Reply::any(), State::#state{}, hibernate | infinity | non_neg_integer()} |
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), Reply::any(), State::#state{}} |
    {stop, Reason::any(), State::#state{}}.

handle_call({access_list_set, NS, Jid} = Info, _From, State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, is_allowed(set, NS, Jid, State), State, get_countdown(State)};

handle_call({access_list_get, NS, Jid} = Info, _From, State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, is_allowed(get, NS, Jid, State), State, get_countdown(State)};

handle_call(reconnect, _From, State) ->
    exmpp_component:stop(State#state.xmppCom),
    timer:sleep(250),
    {_, XmppCom} = make_connection(
        State#state.jid, State#state.pass,
        State#state.server, State#state.port
    ),
    {reply, ok, State#state{xmppCom=XmppCom}, get_countdown(State)};

handle_call({change_config, syslog, {Facility, Name}}, _From, State) ->
    init_syslog(if
        is_number(Facility) -> Facility;
        true -> list_to_atom(Facility)
    end, Name),
    {reply, ok, State, get_countdown(State)};

handle_call({change_config, Key, Value}, _From, State) ->
    case Key of
        maxPerPeriod -> {reply, ok, State#state{maxPerPeriod=Value}, get_countdown(State)};
        periodSeconds -> {reply, ok, State#state{periodSeconds=Value}, get_countdown(State)};
        maxTries -> {reply, ok, State#state{maxTries=Value}, get_countdown(State)};
        resendPeriod -> {reply, ok, State#state{resendPeriod=Value}, get_countdown(State)};
        requestTimeout -> {reply, ok, State#state{requestTimeout=Value}, get_countdown(State)};
        _ -> {reply, error, State, get_countdown(State)}
    end;

handle_call({set_xmpp_conf, JID, Pass, Server, Port}, _From, State) ->
    exmpp_component:stop(State#state.xmppCom),
    timer:sleep(250),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    {reply, ok, State#state{jid=JID, pass=Pass, server=Server, port=Port, xmppCom=XmppCom}, get_countdown(State)};

handle_call(get_xmpp_conf, _From, State) ->
    {reply, [State#state.jid, State#state.pass, State#state.server, State#state.port], State, get_countdown(State)};
        
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Info, _From, State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, ok, State, get_countdown(State)}.


-spec terminate(Reason::any(), State::#state{}) -> ok.

terminate(_Reason, _State) ->
    lager:info("Terminated Component.", []),
    ok.

-spec code_change(OldVsn::string(), State::#state{}, Extra::any()) ->
    {ok, State::#state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec reset_countdown(State::#state{}) -> #state{}.

reset_countdown(State) ->
    {A,B,_} = now(),
    State#state{timeout=(A*1000000+B)}.

-spec get_countdown(Begin::integer()) -> integer().

get_countdown(#state{timeout=undefined}) ->
    100;
get_countdown(#state{timeout=Begin,requestTimeout=RT}) ->
    {A,B,_} = now(),
    case ((A * 1000000 + B) - Begin) of
        Time when (RT - Time) > 0 ->
            (RT - Time) * 1000;
        _ ->
            100
    end.

-spec configure() -> {ok, #state{}}.

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
    {ok, reset_countdown(#state{
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
    })}.

-spec gen_id() -> binary().

gen_id() ->
    list_to_binary(uuid:to_string(uuid:uuid4())).

-spec expired_stanzas(Timeout::integer()) -> ok.

expired_stanzas(Timeout) ->
    [ resend(N) || {_K, N} <- timem:remove_expired(Timeout), is_record(N, matching) ],
    ok.

-spec resend(N :: #matching{}) -> ok.

resend(N) when is_record(N, matching) ->
    ?MODULE ! {resend, N},
    ok.

-spec init_syslog(Facility::(atom() | integer()), Name::string()) -> ok | {error, Reason::any()}.

init_syslog(Facility, Name) ->
    lager:info("Syslog configured: facility=~p, name=~p",[Facility, Name]),
    syslog:open(Name, [cons, perror, pid], Facility).

-spec save_id(Id::binary(), NS::string(), Packet::term(), App::atom()) -> #matching{} | ok.

save_id(_Id, _NS, _Packet, ecomponent) ->
    ok;

save_id(Id, NS, Packet, App) ->
    N = #matching{id=Id, ns=NS, processor=App, tries=0, packet=Packet},
    save_id(N).

-spec save_id(N::#matching{}) -> #matching{} | ok.
    
save_id(#matching{id=Id, processor=App}=N) ->
    case timem:insert(Id, N) of
        true ->
            N;
        _ ->
            lager:error("Error writing id ~s, processor ~p on timem, reason: ~p", [Id, App])
    end.

-spec get_processor(Id::binary()) -> #matching{} | undefined.

get_processor(Id) ->
    case timem:remove(Id) of
        {_, N} when is_record(N, matching) -> 
            N;
        _ ->
            lager:warning("Found no matching processor for id [~p]",[Id]),
            undefined
    end.

-spec prepare_processors(P::list(processor())) -> ok.

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

-spec get_processor_by_ns(NS::atom()) -> [] | mod_processor() | app_processor().

get_processor_by_ns(NS) ->
    case ets:lookup(?NS_PROCESSOR, NS) of
        [{_, {_T, _P}=Result}] -> Result;
        _ ->
            case ets:lookup(?NS_PROCESSOR, default) of
                [{_, {_T, _P}=Result}] -> Result;
                _ -> []
            end
    end.

-spec make_connection(JID::maybe_improper_list(), Pass::maybe_improper_list(), Server::maybe_improper_list(), Port::integer()) -> {R::string(), XmppCom::pid()}.

make_connection(JID, Pass, Server, Port) -> 
    XmppCom = exmpp_component:start(),
    make_connection(XmppCom, JID, Pass, Server, Port, 20).
    
-spec make_connection(XmppCom::pid(), JID::jid(), Pass::string(), Server::string(), Port::integer(), Tries::integer()) -> {string(), pid()}.    

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

-spec prepare_id(Data::string()) -> string().

prepare_id([]) -> [];
prepare_id([$<|T]) -> [$x|prepare_id(T)];
prepare_id([$>|T]) -> [$X|prepare_id(T)];
prepare_id([H|T]) -> [H|prepare_id(T)].

-spec unprepare_id(Data::string()) -> string().

unprepare_id([]) -> [];
unprepare_id([$x|T]) -> [$<|unprepare_id(T)];
unprepare_id([$X|T]) -> [$>|unprepare_id(T)];
unprepare_id([H|T]) -> [H|unprepare_id(T)].

-spec send(Packet::term(), App::atom()) -> ok.

send(Packet, App) ->
    Payload = exmpp_iq:get_payload(Packet),
    NS = exmpp_xml:get_ns_as_atom(Payload),
    send(Packet, NS, App).

-spec send(Packet::term(), NS::atom(), App::atom()) -> ok.

send(Packet, NS, App) ->
    ?MODULE ! {send, Packet, NS, App},
    ok.

-spec is_allowed( (set | get | error | result), NS::atom(), JID::jid(), State::#state{}) -> boolean().

is_allowed(set, NS, {_, Domain, _}, #state{accessListSet=As}) ->
    is_allowed(NS, Domain, As);
is_allowed(get, NS, {_, Domain, _}, #state{accessListGet=Ag}) ->
    is_allowed(NS, Domain, Ag).

-spec is_allowed( NS::atom(), Domain::string(), PList::list(binary()) ) -> boolean().

is_allowed(NS, Domain, PList) ->
    case proplists:get_value(NS, PList) of
        undefined ->
            true;
        List ->
            lists:member(Domain, List)
    end.

-type levels() :: emerg | alert | crit | err | warning | notice | info | debug.

-spec syslog(Level::levels(), Message::string()) -> ok.

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

