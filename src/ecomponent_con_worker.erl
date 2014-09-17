-module(ecomponent_con_worker).
-behaviour(gen_server).

-compile([warnings_as_errors]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("ecomponent.hrl").

-record(state, {
    type = server :: server | node,
    xmppCom :: pid(),
    jid :: ecomponent:jid(),
    id :: atom(),
    pass :: string(),
    server :: string(),
    port :: integer(),
    node :: atom(),
    group :: atom(),
    conn_type = active :: active | passive
}).

%% gen_server callbacks
-export([
    start_link/3, 
    stop/1, 
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-spec start_link(ID::{atom(),atom()}, JID::ecomponent:jid(), Conf::proplists:proplist()) ->
    {ok,pid()} | ignore | {error,{already_started,pid()}} | {error, term()}.
%@doc Starts an individual connection. The connection can be to a server or
%     another node in the cluster.
%@end
start_link({ID,Group}, JID, Conf) ->
    gen_server:start_link({local, ID}, ?MODULE, [{ID,Group}, JID, Conf], []).

-spec stop(ID::atom()) -> ok.
%@doc Stops an individual connection.
%@end
stop(ID) ->
    gen_server:call(ID, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%@hidden
init([{ID, Group}, JIDdefault, Conf]) ->
    Pass = proplists:get_value(pass, Conf),
    Server = proplists:get_value(server, Conf),
    Port = proplists:get_value(port, Conf),
    JID = proplists:get_value(jid, Conf, JIDdefault),
    F = proplists:get_value(type, Conf, active),
    case Server of
        undefined ->
            Node = proplists:get_value(node, Conf),
            erlang:monitor_node(Node, true),
            ecomponent_con:F(ID, Group),
            {ok, #state{type = node, node = Node, group=Group}};
        _ ->
            {_, XmppCom} = make_connection(JID, Pass, Server, Port),
            ecomponent_con:F(ID, Group),
            {ok, #state{
                type = server,
                xmppCom = XmppCom,
                id = ID,
                jid = JID,
                pass = Pass,
                server = Server,
                port = Port,
                group = Group,
                conn_type = F
            }}
    end.


-spec handle_info(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), State::#state{}}.
%@hidden
handle_info(#received_packet{from=To,id=ID}=ReceivedPacket, State) ->
    lager:debug("Received Packet: ~p~n", [ReceivedPacket]),
    ToBin = exmpp_jid:bare_to_binary(exmpp_jid:make(To)),
    case ReceivedPacket#received_packet.packet_type of
        iq -> timem:insert({ID, ToBin}, State#state.group);
        _ -> ok
    end,
    ecomponent ! {ReceivedPacket, State#state.group},
    {noreply, State};

handle_info({send, #xmlel{name='iq'}=Packet}, #state{type=node, jid=JID, group=ID, node=Node}=State) ->
    From = exmpp_stanza:get_sender(Packet),
    NewPacket = case From of
        undefined ->
            exmpp_xml:set_attribute(Packet, <<"from">>, JID);
        _ ->
            Packet
    end,
    rpc:cast(Node, ecomponent, send, [NewPacket, 'from_another_node', undefined, false, ID]),
    {noreply, State};

handle_info({send, #xmlel{name='message'}=Packet}, #state{type=node, jid=JID, group=ID, node=Node}=State) ->
    From = exmpp_stanza:get_sender(Packet),
    NewPacket = case From of
        undefined ->
            exmpp_xml:set_attribute(Packet, <<"from">>, JID);
        _ ->
            Packet
    end,
    rpc:cast(Node, ecomponent, send_message, [NewPacket, ID]),
    {noreply, State};

handle_info({send, #xmlel{name='presence'}=Packet}, #state{type=node, jid=JID, group=ID, node=Node}=State) ->
    From = exmpp_stanza:get_sender(Packet),
    NewPacket = case From of
        undefined ->
            exmpp_xml:set_attribute(Packet, <<"from">>, JID);
        _ ->
            Packet
    end,
    rpc:cast(Node, ecomponent, send_presence, [NewPacket, ID]),
    {noreply, State};

handle_info({send, Packet}, #state{xmppCom=XmppCom, jid=JID}=State) ->
    From = exmpp_stanza:get_sender(Packet),
    NewPacket = case From of
        undefined ->
            exmpp_xml:set_attribute(Packet, <<"from">>, JID);
        _ ->
            Packet
    end,
    exmpp_component:send_packet(XmppCom, NewPacket),
    {noreply, State};

handle_info({down, Node}, #state{node=Node, conn_type=F}=State) ->
    lager:info("Connection to ~p closed. Trying to reconnect...~n", [Node]),
    ecomponent_con:down(State#state.id),
    case net_kernel:connect_node(Node) of
    true ->
        lager:info("Reconnected ~p.~n", [Node]),
        ecomponent_con:F(State#state.id, State#state.group);
    false ->
        timer:sleep(500)
    end,
    {noreply, State};

handle_info({_, tcp_closed}, #state{jid=JID, server=Server, pass=Pass, port=Port, conn_type=F}=State) ->
    lager:info("Connection to ~s closed. Trying to reconnect...~n", [Server]),
    ecomponent_con:down(State#state.id),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    lager:info("Reconnected ~s.~n", [Server]),
    ecomponent_con:F(State#state.id, State#state.group),
    {noreply, State#state{xmppCom=XmppCom}};

handle_info({_,{bad_return_value, _}}, #state{jid=JID, server=Server, pass=Pass, port=Port, conn_type=F}=State) ->
    lager:info("Connection to ~s closed. Trying to reconnect...~n", [Server]),
    ecomponent_con:down(State#state.id),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    lager:info("Reconnected ~s.~n", [Server]),
    ecomponent_con:F(State#state.id, State#state.group),
    {noreply, State#state{xmppCom=XmppCom}};

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
handle_call(stop, _From, #state{xmppCom=XmppCom}=State) ->
    lager:info("Component Stopped.~n",[]),
    exmpp_component:stop(XmppCom),
    {stop, normal, ok, State};

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

-spec make_connection(JID::string(), Pass::string(), Server::string(), Port::integer()) -> {R::string(), XmppCom::pid()}.
%@hidden
make_connection(JID, Pass, Server, Port) -> 
    make_connection(JID, Pass, Server, Port, 20).
    
-spec make_connection(JID::ecomponent:jid(), Pass::string(), Server::string(), Port::integer(), Tries::integer()) -> {string(), pid()}.    
%@hidden
make_connection(JID, Pass, Server, Port, 0) -> 
    make_connection(JID, Pass, Server, Port);
make_connection(JID, Pass, Server, Port, Tries) ->
    lager:info("Connecting: ~p Tries Left~n",[Tries]),
    XmppCom = exmpp_component:start(),
    try setup_exmpp_component(XmppCom, JID, Pass, Server, Port) of
        R -> 
            lager:info("Connected.~n",[]),
            {R, XmppCom}
    catch
        Class:Exception ->
            lager:warning("Exception ~p: ~p~n",[Class, Exception]),
            exmpp_component:stop(XmppCom),
            clean_exit_normal(),
            timer:sleep((20-Tries) * 200),
            make_connection(JID, Pass, Server, Port, Tries-1)
    end.

-spec setup_exmpp_component(XmppCom::pid(), JID::ecomponent:jid(), Pass::string(), Server::string(), Port::integer()) -> string().
%@hidden
setup_exmpp_component(XmppCom, JID, Pass, Server, Port)->
    exmpp_component:auth(XmppCom, JID, Pass),
    exmpp_component:connect(XmppCom, Server, Port),
    exmpp_component:handshake(XmppCom).

-spec clean_exit_normal() -> ok.
%@hidden
clean_exit_normal() ->
    receive 
        {_Ref, normal} -> ok
    after 500 -> ok
    end.
