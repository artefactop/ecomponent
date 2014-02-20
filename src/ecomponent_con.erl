-module(ecomponent_con).
-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("ecomponent.hrl").

-record(state, {
    active  = [] :: [atom()],
    passive = [] :: [atom()],
    down    = [] :: [atom()]
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
    send/1,
    send/2,
    active/1,
    passive/1,
    down/1
]).

-spec is_active(ID::atom()) -> boolean().

is_active(ID) ->
    gen_server:call(?MODULE, {is_active, ID}).

-spec send(Info::exmpp_xml:xmlel()) -> ok.

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

send(Info, ID) ->
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

start_link(JID, Conf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [JID, Conf], []).


-spec stop() -> ok.

stop() ->
    gen_server:call(?MODULE, stop).

active(ID) ->
    ?MODULE ! {active, ID}.

passive(ID) ->
    ?MODULE ! {passive, ID}.

down(ID) ->
    ?MODULE ! {down, ID}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([JID, Conf]) ->
    case proplists:get_value(servers, Conf) of 
        undefined ->
            spawn(fun() -> ecomponent_con_sup:start_child(default, JID, Conf) end);
        SrvInfo ->
            lists:foreach(fun({ID, SrvConf}) ->
                spawn(fun() -> 
                    ecomponent_con_sup:start_child(ID, JID, SrvConf)
                end)
            end, SrvInfo)
    end,
    {ok, #state{}}.

-spec handle_info(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), State::#state{}}.

handle_info({active, X}, #state{active=Pools, passive=Passive, down=Down}=State) ->
    {noreply, State#state{active=Pools ++ [X], passive=Passive -- [X], down=Down -- [X]}};

handle_info({passive, X}, #state{active=Pools, passive=Passive, down=Down}=State) ->
    {noreply, State#state{active=Pools -- [X], passive=Passive ++ [X], down=Down -- [X]}};

handle_info({down, X}, #state{active=Pools, passive=Passive, down=Down}=State) ->
    {noreply, State#state{active=Pools -- [X], passive=Passive -- [X], down=Down ++ [X]}};

handle_info(Record, State) -> 
    lager:info("Unknown Info Request: ~p~n", [Record]),
    {noreply, State}.

-spec handle_cast(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), State::#state{}}.

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

handle_call(Info, _From, State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, ok, State}.


-spec terminate(Reason::any(), State::#state{}) -> ok.

terminate(_Reason, _State) ->
    lager:info("terminated connection.", []),
    ok.

-spec code_change(OldVsn::string(), State::#state{}, Extra::any()) ->
    {ok, State::#state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

