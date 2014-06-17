-module(ecomponent_con).
-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("ecomponent.hrl").

-record(state, {
    active  = [] :: [atom()],
    passive = [] :: [atom()],
    down    = [] :: [atom()],
    groups  = [] :: [{atom(), [atom()]}]
}).

-define(TIME_TO_SLEEP, 2000).

%% gen_server callbacks
-export([
    start_link/2, 
    stop/0, 
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    is_active/1,
    resolve_id/1,
    send/1,
    send/2,
    active/1,
    active/2,
    passive/1,
    passive/2,
    down/1
]).

-spec is_active(ID::atom()) -> boolean().
%@doc Checks an ID. If the connection is available as passive or down
%     returns true, otherwise false.
%@end
is_active(ID) ->
    gen_server:call(?MODULE, {is_active, ID}).

-spec resolve_id(ID::atom()) -> atom().
%@doc Resolve from group ID. If the ID passed as param is a group, this
%     function returns an element inside the group in round-robin. Else,
%     you get the same atom.
%@end
resolve_id(ID) ->
    gen_server:call(?MODULE, {resolve_id, ID}).

-spec send(Info::exmpp_xml:xmlel()) -> ok.
%@doc Select a connection and send the stanza.
%@end
send(Info) ->
    ToBin = case exmpp_stanza:get_recipient(Info) of 
        undefined -> 
            undefined;
        To -> 
            exmpp_jid:bare_to_binary(exmpp_jid:parse(To))
    end,
    ID = exmpp_stanza:get_id(Info),  
    send(Info, timem:remove({ID,ToBin})).

-spec send(Info::exmpp_xml:xmlel(), ID::atom()) -> ok.
%@doc Send the stanza to the specific connection. If the connection is not
%     available the stanza will be sent to the first available connection
%     in the active pool. If the active pool is empty, try to send the
%     stanza to a passive connection. And finally if this is impossible,
%     waits a moment (2000ms) and try again.
%@end
send(Info, RawID) ->
    ID = resolve_id(RawID),
    case ID =/= undefined andalso is_active(ID) of 
        true ->
            lager:debug("Connecting to (fix) pool: ~p~n", [ID]),
            ID ! {send, Info},
            ok;
        _ ->
            case catch gen_server:call(?MODULE, get_pool) of 
                {'EXIT', _} ->
                    lager:error("ecomponent_con DOWN... waiting..."),
                    timer:sleep(?TIME_TO_SLEEP),
                    send(Info);
                [] ->
                    lager:error("no pool active to send the packet... waiting..."),
                    timer:sleep(?TIME_TO_SLEEP),
                    send(Info);
                Pool ->
                    lager:debug("Connecting to pool: ~p~n", [Pool]),
                    Pool ! {send, Info},
                    ok
            end
    end.

-spec start_link(JID::ecomponent:jid(), Conf::proplists:proplist()) ->
    {ok,pid()} | ignore | {error,{already_started,pid()}} | {error, term()}.
%@doc Starts the connection. The params needed to start the connection are a JID
%     in string format and a proplists of configurations with server or node
%     keys available inside, and others configurations more.
%@end
start_link(JID, Conf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [JID, Conf], []).


-spec stop() -> ok.
%@doc Stops the connection.
stop() ->
    gen_server:call(?MODULE, stop).

-spec active(ID :: atom()) -> {active, atom()}.
%@doc Sets the connection active. This function will be used for activate
%     the connections.
%@end
active(ID) ->
    ?MODULE ! {active, ID}.

-spec active(ID :: atom(), Group :: atom()) -> {active, atom()}.
%@doc Sets the connection active. This function will be used for activate
%     the connections. Adds a new parameter named Group for use only this
%     name instead of the specific (or generated) names in case of pool
%     is used.
%@end
active(ID, Group) ->
    ?MODULE ! {active, ID, Group}.

-spec passive(ID :: atom()) -> {passive, atom()}.
%@doc Sets the connection passive. This function will be used for activate
%     as passive a connection.
%@end
passive(ID) ->
    ?MODULE ! {passive, ID}.

-spec passive(ID :: atom(), Group :: atom()) -> {passive, atom()}.
%@doc Sets the connection passive. This function will be used for activate
%     as passive a connection. Adds a new parameter named Group for use only 
%     this name instead of the specific (or generated) names in case of pool
%     is used.
%@end
passive(ID, Group) ->
    ?MODULE ! {passive, ID, Group}.

-spec down(ID :: atom()) -> {down, atom()}.
%@doc Sets the connection as down. This function will be used to report a
%     down from the worker connection.
%@end
down(ID) ->
    ?MODULE ! {down, ID}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%@hidden
init([JID, Conf]) ->
    case proplists:get_value(servers, Conf) of 
        undefined ->
            spawn(fun() -> ecomponent_con_sup:start_child(default, JID, Conf) end);
        SrvInfo ->
            lists:foreach(fun({_ID, SrvConf}=SrvTerm) ->
                spawn(fun() ->
                    PoolNum = proplists:get_value(poolsize, SrvConf, 1),
                    init_worker(PoolNum, JID, SrvTerm)
                end)
            end, SrvInfo)
    end,
    {ok, #state{}}.

-spec handle_info(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), State::#state{}}.
%@hidden
handle_info({active, X}, #state{
        active=Pools, passive=Passive, down=Down}=State) ->
    {noreply, State#state{
        active=Pools ++ [X], 
        passive=Passive -- [X], 
        down=Down -- [X]}};

handle_info({active, X, Group}, #state{
        active=Pools, passive=Passive, down=Down, groups=Groups}=State) ->
    {noreply, State#state{
        active=Pools ++ [X], 
        passive=Passive -- [X], 
        down=Down -- [X],
        groups=add_to_group(X, Group, Groups)}};

handle_info({passive, X}, #state{
        active=Pools, passive=Passive, down=Down}=State) ->
    {noreply, State#state{
        active=Pools -- [X], 
        passive=Passive ++ [X], 
        down=Down -- [X]}};

handle_info({passive, X, Group}, #state{
        active=Pools, passive=Passive, down=Down, groups=Groups}=State) ->
    {noreply, State#state{
        active=Pools -- [X], 
        passive=Passive ++ [X], 
        down=Down -- [X],
        groups=add_to_group(X, Group, Groups)}};

handle_info({down, X}, #state{
        active=Pools, passive=Passive, down=Down, groups=Groups}=State) ->
    {noreply, State#state{
        active=Pools -- [X],
        passive=Passive -- [X], 
        down=Down ++ [X],
        groups=remove_from_groups(X, Groups)}};

handle_info(Record, State) -> 
    lager:info("Unknown Info Request: ~p~n", [Record]),
    {noreply, State}.

-spec handle_cast(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), State::#state{}}.
%@hidden
handle_cast(_Msg, State) ->
    lager:info("Received: ~p~n", [_Msg]), 
    {noreply, State}.


-spec handle_call(Msg::any(), From::{pid(),_}, State::#state{}) ->
    {reply, Reply::any(), State::#state{}} |
    {reply, Reply::any(), State::#state{}, hibernate | infinity | non_neg_integer()} |
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), Reply::any(), State::#state{}} |
    {stop, Reason::any(), State::#state{}}.
%@hidden
handle_call(stop, _From, #state{active=Pools, down=Down}=State) ->
    [ ecomponent_con_worker:stop(Pool) || Pool <- Pools ],
    [ ecomponent_con_worker:stop(Pool) || Pool <- Down ],
    {stop, normal, ok, State};

handle_call({is_active, ID}, _From, #state{active=Pools, passive=Passive}=State) ->
    {reply, lists:member(ID, Pools) orelse lists:member(ID, Passive), State};

handle_call(get_pool, _From, #state{active=[], passive=[Pool|Pools]}=State) ->
    {reply, Pool, State#state{passive=Pools ++ [Pool]}};

handle_call(get_pool, _From, #state{active=[Pool|Pools]}=State) ->
    {reply, Pool, State#state{active=Pools ++ [Pool]}};

handle_call({resolve_id, Group}, _From, #state{groups=Groups}=State) ->
    case proplists:get_value(Group, Groups, []) of
    [] -> 
        {reply, Group, State};
    [ID|Elements] ->
        {reply, ID, State#state{groups = 
            proplists:delete(Group, Groups)
            ++ [{Group, Elements ++ [ID]}]}}
    end;

handle_call(Info, _From, State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, ok, State}.


-spec terminate(Reason::any(), State::#state{}) -> ok.
%@hidden
terminate(_Reason, _State) ->
    lager:info("terminated connection.", []),
    ok.

-spec code_change(OldVsn::string(), State::#state{}, Extra::any()) ->
    {ok, State::#state{}}.
%@hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec init_worker(Seq::pos_integer(), JID::binary(), 
    {ID::atom(), SrvConf::proplists:proplist()}) -> ok.
%@hidden
init_worker(0, _JID, {_ID, _SrvConf}) ->
    ok;

init_worker(Seq, JID, {ID, SrvConf}) when is_integer(Seq) ->
    Name = list_to_atom(
        "conn_" ++
        atom_to_list(ID) ++ "_" ++ 
        integer_to_list(Seq)),
    ecomponent_con_sup:start_child({Name,ID}, JID, SrvConf),
    init_worker(Seq-1, JID, {ID, SrvConf}).

-spec add_to_group(Element::atom(), Group::atom(), Groups::[{atom(), [atom()]}]) ->
    [{atom(), [atom()]}].
%@hidden
add_to_group(Element, Group, Groups) ->
    case proplists:get_value(Group, Groups) of
    undefined -> 
        Groups ++ [{Group, [Element]}];
    Elements ->
        case lists:member(Element, Elements) of
        false -> 
            proplists:delete(Group, Groups)
                ++ [{Group, [Element|Elements]}];
        true -> 
            Groups
        end
    end.

-spec remove_from_groups(X :: atom(), Groups::[{atom(), [atom()]}]) ->
    [{atom(), [atom()]}].
%@hidden
remove_from_groups(X, Groups) ->
    lists:map(fun({Group, Elements}) ->
        {Group, [ E || E <- Elements, X =/= E ]}
    end, Groups).
