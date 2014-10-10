%%%-------------------------------------------------------------------
%%% File        : ecomponent.erl
%%% Author      : Jose Luis Navarro <pepe@yuilop.com>
%%%               Manuel Rubio <manuel@yuilop.com>
%%% Description : ecomponent Service - External Component
%%%-------------------------------------------------------------------
-module(ecomponent).
-compile([warnings_as_errors]).

-behaviour(gen_server).

-include("ecomponent_internal.hrl").

%% API
-export([prepare_id/1, unprepare_id/1, get_processor/1, get_processor_by_ns/1,
        get_message_processor/0, get_presence_processor/0, send/5, send/4, 
        send/3, send/2, send_message/1, send_message/2, send_presence/1, 
        send_presence/2, save_id/4, syslog/2, 
        configure/0, gen_id/0, reset_countdown/1, get_countdown/1,
        sync_send/2, sync_send/3]).

%% gen_server callbacks
-export([
    start_link/0, stop/0, init/1, handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3]).

%%====================================================================
%% public API
%%====================================================================

-spec start_link() -> {ok, Pid::pid()} | {error, Reason::any()}.
%@doc Starts the ecomponent main server.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
%@doc Stops the ecomponent main server.
stop() ->
    gen_server:call(?MODULE, stop).

-spec send(Packet::term(), App::atom()) -> ok.
%@doc Send an IQ stanza. The first param is a Packet in exmpp_xml format. 
%     The second param is the app where the IQ is linked, this could be an
%     atom or a PID.
%@end
send(Packet, App) ->
    Payload = exmpp_iq:get_payload(Packet),
    NS = exmpp_xml:get_ns_as_atom(Payload),
    send(Packet, NS, App, true).

-spec send(Packet::term(), NS::atom(), App::atom()) -> ok.
%@doc Send an IQ stanza and set the return path. The same as send/2 but adds
%     a middle param, the NS (namespace) referer to the name space of the 
%     first element inside the iq stanza.
%@end
send(Packet, NS, App) ->
    send(Packet, NS, App, true).

-spec send(Packet::term(), NS::atom(), App::atom(), Reply::boolean()) -> ok.
%@doc Send an IQ stanza, set the return path and if there are reply or not.
%     This function is the same as send/3 but adds a new param. The fourth
%     param is for enable or disable the waiting for the reply.
%@end
send(Packet, NS, App, Reply) ->
    send(Packet, NS, App, Reply, undefined).

-spec send(Packet::term(), NS::atom(), App::atom(), Reply::boolean(), ServerID::atom()) -> ok.
%@doc Send an IQ stanza with return path, reply and the server where to send.
%     This function adds a new param. The fifth param is the name of the
%     connection where the stanza should be sent.
%@end
send(Packet, NS, App, Reply, ServerID) ->
    ?MODULE ! {send, Packet, NS, App, Reply, ServerID},
    ok.

-spec sync_send(Packet::term(), NS::atom()) -> #params{} | {error, timeout}.
%@doc Send a packet and wait for the reply.
%     Send a packet as do send/3, but the return path is the current process
%     for get the response. The first param is a Packet in the exmpp_xml format.
%     The second param is the namespace (NS).
%@end
sync_send(Packet, NS) ->
    sync_send(Packet, NS, undefined).

-spec sync_send(Packet::term(), NS::atom(), ServerID::atom()) -> #params{} | {error, timeout}.
%@doc Send a packet and wait for the reply using a specific server to send.
%     As in send/3, but the return path is the current process for get the
%     response.
%@end 
sync_send(Packet, NS, ServerID) ->
    send(Packet, NS, self(), true, ServerID),
    receive 
        #response{params=Params=#params{type=Type}}
                when Type =:= "result"
                orelse Type =:= "error" ->
            Params
    after 5000 ->
        {error, timeout}
    end.

-spec send_message(Packet::term()) -> ok.
%@doc Send a message stanza. The Packet should be in exmpp_xml format.
%@end
send_message(Packet) ->
    send_message(Packet, undefined).

-spec send_message(Packet::term(), ServerID::atom()) -> ok.
%@doc Send a message stanza. The Packet should be in exmpp_xml format.
%     This function adds the possibility to select the server for send
%     the stanza.
%@end
send_message(Packet, ServerID) ->
    ?MODULE ! {send_message, Packet, ServerID},
    ok.

-spec send_presence(Packet::term()) -> ok.
%@doc Send a presence stanza. The Packet should be in exmpp_xml format.
%@end
send_presence(Packet) ->
    send_presence(Packet, undefined).

-spec send_presence(Packet::term(), ServerID::atom()) -> ok.
%@doc Send a presence stanza. The Packet should be in exmpp_xml format.
%     This function adds the possibility to select the server for send
%     the stanza.
%@end
send_presence(Packet, ServerID) ->
    ?MODULE ! {send_presence, Packet, ServerID},
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init( Args :: [] ) -> 
    {ok, State :: #state{}} | 
    {ok, State :: #state{}, hibernate | infinity | non_neg_integer()} |
    ignore | {stop, Reason :: string()}.
%@hidden
init([]) ->
    lager:info("Loading Application eComponent", []),
    configure().

-spec handle_info(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), State::#state{}}.
%@hidden
handle_info(
        {#received_packet{packet_type=iq}=ReceivedPacket, ServerID},
        #state{
            maxPerPeriod=MaxPerPeriod, periodSeconds=PeriodSeconds, 
            features=Features,info=Info,disco_info=DiscoInfo,
            throttle=Throttle
        }=State) ->
    #received_packet{
        type_attr=Type, 
        raw_packet=IQ, 
        from={Node, Domain, _}=From}=ReceivedPacket,
    NS = exmpp_iq:get_payload_ns_as_atom(IQ),
    ecomponent_metrics:notify_throughput_iq(in, Type, NS),
    JIDBin = list_to_binary(exmpp_jid:to_list(Node, Domain)),
    NoDropPacket = case Throttle of
        true -> mod_monitor:accept(JIDBin, MaxPerPeriod, PeriodSeconds);
        false -> true
    end,
    if NoDropPacket ->
        ecomponent_metrics:set_iq_time(exmpp_stanza:get_id(IQ), Type, NS),
        if
            (DiscoInfo =:= false) andalso (?NS_DISCO_INFO =:= NS) ->
                lager:debug("Ignored by disco#info muted! NS=~p~n", [NS]),
                ignore;
            true ->
                lager:debug("To process packet with NS=~p~n", [NS]),
                process_run(iq_handler, pre_process_iq, [
                    Type, IQ, NS, From, Features, Info, ServerID])
        end,
        {noreply, State, get_countdown(State)};
    true ->
        ecomponent_metrics:notify_dropped_iq(Type, NS),
        {noreply, State, get_countdown(State)}
    end;

handle_info(
        {#received_packet{packet_type=message}=ReceivedPacket, ServerID}, 
        #state{
            maxPerPeriod=MaxPerPeriod, throttle=Throttle,
            periodSeconds=PeriodSeconds}=State) ->
    #received_packet{
        type_attr=Type, 
        raw_packet=Message, 
        from={Node, Domain, _}=From}=ReceivedPacket,
    ecomponent_metrics:notify_throughput_message(in, Type),
    JIDBin = list_to_binary(exmpp_jid:to_list(Node, Domain)),
    NoDropPacket = case Throttle of
        true -> mod_monitor:accept(JIDBin, MaxPerPeriod, PeriodSeconds);
        false -> true
    end,
    if NoDropPacket ->
        process_run(message_handler, pre_process_message, [
            Type, Message, From, ServerID]),
        {noreply, State, get_countdown(State)};
    true ->
        ecomponent_metrics:notify_dropped_message(Type),
        {noreply, State, get_countdown(State)}
    end;


handle_info(
        {#received_packet{packet_type=presence}=ReceivedPacket, ServerID},
        #state{
            maxPerPeriod=MaxPerPeriod, throttle=Throttle,
            periodSeconds=PeriodSeconds}=State) ->
    #received_packet{
        type_attr=Type, 
        raw_packet=Presence, 
        from={Node, Domain, _}=From}=ReceivedPacket,
    ecomponent_metrics:notify_throughput_presence(in, Type),
    JIDBin = list_to_binary(exmpp_jid:to_list(Node, Domain)),
    NoDropPacket = case Throttle of
        true -> mod_monitor:accept(JIDBin, MaxPerPeriod, PeriodSeconds);
        false -> true
    end,
    if NoDropPacket ->
        process_run(presence_handler, pre_process_presence, [
            Type, Presence, From, ServerID]),
        {noreply, State, get_countdown(State)};
    true ->
        ecomponent_metrics:notify_dropped_presence(Type),
        {noreply, State, get_countdown(State)}
    end;

handle_info({send, OPacket, NS, App, Reply, ServerID}, State) ->
    Kind = exmpp_iq:get_kind(OPacket),
    Packet = case exmpp_stanza:get_id(OPacket) of
        undefined ->
            ID = gen_id(),
            exmpp_xml:set_attribute(OPacket, <<"id">>, ID);
        _ -> 
            OPacket
    end,
    case Kind of
        request when Reply =:= false ->
            ecomponent_metrics:notify_throughput_iq(
                out, exmpp_iq:get_type(Packet), NS);
        request ->
            ecomponent_metrics:notify_throughput_iq(
                out, exmpp_iq:get_type(Packet), NS),
            save_id(exmpp_stanza:get_id(Packet), NS, Packet, App);
        _ -> 
            ecomponent_metrics:notify_resp_time(exmpp_stanza:get_id(Packet))
    end,
    lager:debug("Sending packet ~p",[Packet]),
    case ServerID of 
        undefined -> ecomponent_con:send(Packet);
        _ -> ecomponent_con:send(Packet, ServerID)
    end,
    {noreply, State, get_countdown(State)};

handle_info({send_message, OPacket, ServerID}, State) ->
    Packet = case exmpp_stanza:get_id(OPacket) of
        undefined ->
            ID = gen_id(),
            exmpp_xml:set_attribute(OPacket, <<"id">>, ID);
        _ -> 
            OPacket
    end,
    lager:debug("Sending packet ~p",[Packet]),
    Type = case exmpp_stanza:get_type(Packet) of
        undefined -> <<"normal">>;
        T -> T
    end, 
    ecomponent_metrics:notify_throughput_message(out, Type),
    case ServerID of
        undefined -> ecomponent_con:send(Packet);
        _ -> ecomponent_con:send(Packet, ServerID)
    end,
    {noreply, State, get_countdown(State)};


handle_info({send_presence, OPacket, ServerID}, State) ->
    Packet = case exmpp_stanza:get_id(OPacket) of
    undefined ->
        ID = gen_id(),
        exmpp_xml:set_attribute(OPacket, <<"id">>, ID);
    _ -> 
        OPacket
    end,
    lager:debug("Sending packet ~p",[Packet]),
    Type = case exmpp_stanza:get_type(Packet) of
        undefined -> <<"available">>;
        T -> T
    end, 
    ecomponent_metrics:notify_throughput_presence(out, Type),
    case ServerID of 
        undefined -> ecomponent_con:send(Packet);
        _ -> ecomponent_con:send(Packet, ServerID)
    end,
    {noreply, State, get_countdown(State)};

handle_info({
        resend, 
        #matching{tries=Tries, packet=P}=N}, 
        #state{maxTries=Max}=State) when Tries < Max ->
    save_id(N#matching{tries=Tries+1}),
    ecomponent_con:send(P), 
    {noreply, State, get_countdown(State)};

handle_info({
        resend, 
        #matching{tries=Tries}=N}, 
        #state{maxTries=Max}=State) when Tries >= Max ->
    lager:warning("Max tries exceeded for: ~p~n", [N]),
    {noreply, State, get_countdown(State)};

handle_info(timeout, #state{resend=Resend,requestTimeout=RT}=State) ->
    expired_stanzas(Resend,RT),
    {noreply, reset_countdown(State), State#state.requestTimeout * 1000};

handle_info(Record, State) -> 
    lager:info("Unknown Info Request: ~p~n", [Record]),
    {noreply, State, get_countdown(State)}.

-spec handle_cast(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), State::#state{}}.
%@hidden
handle_cast(_Msg, State) ->
    lager:info("Received: ~p~n", [_Msg]), 
    {noreply, State, get_countdown(State)}.


-spec handle_call(Msg::any(), From::{pid(),_}, State::#state{}) ->
    {reply, Reply::any(), State::#state{}} |
    {reply, Reply::any(), State::#state{}, hibernate | infinity | 
    non_neg_integer()} |
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), Reply::any(), State::#state{}} |
    {stop, Reason::any(), State::#state{}}.
%@hidden
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
        requestTimeout -> {reply, ok, State#state{requestTimeout=Value}, get_countdown(State)};
        _ -> {reply, error, State, get_countdown(State)}
    end;

handle_call(message_processor, _From, State) ->
    {reply, State#state.message_processor, State, get_countdown(State)};

handle_call(presence_processor, _From, State) ->
    {reply, State#state.presence_processor, State, get_countdown(State)};

handle_call(stop, _From, State) ->
    lager:info("Component Stopped.~n",[]),
    ecomponent_con:stop(),
    {stop, normal, ok, State};

handle_call(Info, _From, State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, ok, State, get_countdown(State)}.


-spec terminate(Reason::any(), State::#state{}) -> ok.
%@hidden
terminate(_Reason, _State) ->
    lager:info("Terminated Component.", []),
    ok.

-spec code_change(OldVsn::string(), State::#state{}, Extra::any()) ->
    {ok, State::#state{}}.
%@hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec waiting_data() -> ok.
%@hidden
waiting_data() ->
    receive
        {M, F, A} -> erlang:apply(M, F, A)
    after 5000 ->
        lager:error("waiting data doesn't receive data!", [])
    end,
    ok.

-spec process_run(M::atom(), F::atom(), A::[any()]) -> ok.
%@hidden
process_run(M, F, A) ->
    PID = spawn(fun waiting_data/0),
    case is_process_alive(PID) of
    true ->
        PID ! {M,F,A};
    false ->
        lager:error("process DIE: ~p:~p ~p", [M,F,A])
    end,
    ok.

-spec reset_countdown(State::#state{}) -> #state{}.
%@hidden
reset_countdown(State) ->
    {A,B,_} = os:timestamp(),
    State#state{timeout=(A*1000000+B)}.

-spec get_countdown(Begin::integer()) -> integer().
%@hidden
get_countdown(#state{resend=false}) ->
    ?REQUEST_TIMEOUT * 1000;
get_countdown(#state{timeout=undefined}) ->
    100;
get_countdown(#state{timeout=Begin,requestTimeout=RT}) ->
    {A,B,_} = os:timestamp(),
    case ((A * 1000000 + B) - Begin) of
        Time when (RT - Time) > 0 ->
            (RT - Time) * 1000;
        _ ->
            100
    end.

-spec configure() -> {ok, #state{}}.
%@hidden
configure() ->
    Conf = application:get_all_env(ecomponent),
    Facility = proplists:get_value(syslog_facility, Conf, local7),
    Name = proplists:get_value(syslog_name, Conf, "ecomponent"),
    init_syslog(Facility, Name),
    [ lager:info("~p = ~p", [X,Y]) || {X,Y} <- Conf, X /= pass ],

    JID = proplists:get_value(jid, Conf),
    WhiteList = proplists:get_value(whitelist, Conf, []),
    Processors = proplists:get_value(processors, Conf, []),

    ecomponent_con:start_link(JID, Conf),
    ecomponent_mnesia:init(Conf),
    ecomponent_metrics:init(Conf),
    Throttle = proplists:get_value(throttle, Conf, true),
    case Throttle of
        true -> mod_monitor:init(WhiteList);
        false -> ok
    end,
    prepare_processors(Processors),
    State = #state{
        jid = JID,
        whiteList = WhiteList,
        maxPerPeriod = proplists:get_value(max_per_period, Conf, ?MAX_PER_PERIOD),
        periodSeconds = proplists:get_value(period_seconds, Conf, ?PERIOD_SECONDS),
        processors = proplists:get_value(processors, Conf),
        message_processor = proplists:get_value(message_processor, Conf, undefined),
        presence_processor = proplists:get_value(presence_processor, Conf, undefined),
        maxTries = proplists:get_value(max_tries, Conf, ?MAX_TRIES),
        requestTimeout = proplists:get_value(request_timeout, Conf, ?REQUEST_TIMEOUT),
        features = proplists:get_value(features, Conf, []),
        info = proplists:get_value(info, Conf, []), 
        disco_info = proplists:get_value(disco_info, Conf, true),
        throttle = Throttle
    },
    {ok, reset_countdown(State), get_countdown(State)}.

-spec gen_id() -> binary().
%@doc generate an ID based on UUID v4.
%@end
gen_id() ->
    list_to_binary(uuid:to_string(uuid:uuid4())).

-spec expired_stanzas(Resend::boolean(), Timeout::integer()) -> ok.
%@hidden
expired_stanzas(false, Timeout) ->
    timem:remove_expired(Timeout),
    ok;

expired_stanzas(true, Timeout) ->
    [ resend(N) || {_K, N} <- timem:remove_expired(Timeout), is_record(N, matching) ],
    ok.

-spec resend(N :: #matching{}) -> ok.
%@hidden
resend(N) when is_record(N, matching) ->
    ?MODULE ! {resend, N},
    ok.

-spec init_syslog(Facility::(atom() | integer()), Name::string()) -> ok | {error, Reason::any()}.
%@hidden
init_syslog(Facility, Name) ->
    lager:info("Syslog configured: facility=~p, name=~p",[Facility, Name]),
    syslog:open(Name, [cons, perror, pid], Facility).

-spec save_id(Id::binary(), NS::string(), Packet::term(), App::atom()) -> #matching{} | ok.
%@hidden
save_id(_Id, _NS, _Packet, ecomponent) ->
    ok;

save_id(Id, NS, Packet, App) ->
    N = #matching{id=Id, ns=NS, processor=App, tries=0, packet=Packet},
    save_id(N).

-spec save_id(N::#matching{}) -> #matching{} | ok.
%@hidden
save_id(#matching{id=Id, processor=App}=N) ->
    case timem:insert(Id, N) of
        true ->
            N;
        _ ->
            lager:error("Error writing id ~s, processor ~p on timem, reason: ~p", [Id, App])
    end.

-spec get_processor(Id::binary()) -> #matching{} | undefined.
%@hidden
get_processor(Id) ->
    case timem:remove(Id) of
        {_, N} when is_record(N, matching) -> 
            N;
        _ ->
            lager:warning("Found no matching processor for id [~p]",[Id]),
            undefined
    end.

-spec prepare_processors(P::list(processor())) -> ok.
%@hidden
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
%@hidden
get_processor_by_ns(NS) ->
    case ets:lookup(?NS_PROCESSOR, NS) of
        [{_, {_T, _P}=Result}] -> Result;
        _ ->
            case ets:lookup(?NS_PROCESSOR, default) of
                [{_, {_T, _P}=Result}] -> Result;
                _ -> undefined
            end
    end.

-spec get_message_processor() -> undefined | mod_processor() | app_processor().
%@hidden
get_message_processor() ->
    PID = whereis(?MODULE),
    case erlang:is_pid(PID) of
        true ->
            gen_server:call(PID, message_processor);
        _ -> 
            syslog(crit, io_lib:format("Process not Alive with Name: ~p~n", [?MODULE]))
    end.

-spec get_presence_processor() -> undefined | mod_processor() | app_processor().
%@hidden
get_presence_processor() ->
    PID = whereis(?MODULE),
    case erlang:is_pid(PID) of
        true ->
            gen_server:call(PID, presence_processor);
        _ -> 
            syslog(crit, io_lib:format("Process not Alive with Name: ~p~n", [?MODULE]))
    end.

-spec prepare_id(Data::string()) -> string().
%@doc Ensure the string used as ID in stanza is XML valid.
%@end
prepare_id([]) -> [];
prepare_id([$<|T]) -> [$x|prepare_id(T)];
prepare_id([$>|T]) -> [$X|prepare_id(T)];
prepare_id([H|T]) -> [H|prepare_id(T)].

-spec unprepare_id(Data::string()) -> string().
%@doc Undo de action of prepare_id/1.
%@end
unprepare_id([]) -> [];
unprepare_id([$x|T]) -> [$<|unprepare_id(T)];
unprepare_id([$X|T]) -> [$>|unprepare_id(T)];
unprepare_id([H|T]) -> [H|unprepare_id(T)].

%@hidden
-type levels() :: emerg | alert | crit | err | warning | notice | info | debug.

-spec syslog(Level::levels(), Message::string()) -> ok.
%@doc Send log info to syslog.
%@end
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
