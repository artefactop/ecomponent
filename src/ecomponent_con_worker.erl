-module(ecomponent_con_worker).
-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("ecomponent.hrl").

-record(state, {
    xmppCom :: pid(),
    jid :: ecomponent:jid(),
    id :: atom(),
    pass :: string(),
    server :: string(),
    port :: integer()
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

start_link(ID, JID, Conf) ->
    gen_server:start_link({local, ID}, ?MODULE, [ID, JID, Conf], []).


-spec stop(ID::atom()) -> ok.

stop(ID) ->
    gen_server:call(ID, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([ID, JID, Conf]) ->
    Pass = proplists:get_value(pass, Conf),
    Server = proplists:get_value(server, Conf),
    Port = proplists:get_value(port, Conf),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    {ok, #state{
        xmppCom = XmppCom,
        id = ID,
        jid = JID,
        pass = Pass,
        server = Server,
        port = Port
    }}.


-spec handle_info(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}} |
    {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} |
    {stop, Reason::any(), State::#state{}}.

handle_info(#received_packet{from=To,id=ID}=ReceivedPacket, State) ->
    lager:debug("Received ~p ~p~n ", [ID, ReceivedPacket]),
    ToBin = exmpp_jid:bare_to_binary(exmpp_jid:make(To)),
    IDBin = erlang:list_to_binary(ID),
    timem:insert({IDBin, ToBin}, State#state.id),
    ecomponent ! {ReceivedPacket, State#state.id},
    {noreply, State};

handle_info({send, Packet}, #state{xmppCom=XmppCom}=State) ->
    exmpp_session:send_packet(XmppCom, Packet),
    {noreply, State};

handle_info({_, tcp_closed}, #state{jid=JID, server=Server, pass=Pass, port=Port}=State) ->
    lager:info("Connection Closed. Trying to Reconnect...~n", []),
    ecomponent_con:down(State#state.id),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    lager:info("Reconnected.~n", []),
    ecomponent_con:active(State#state.id),
    {noreply, State#state{xmppCom=XmppCom}};

handle_info({_,{bad_return_value, _}}, #state{jid=JID, server=Server, pass=Pass, port=Port}=State) ->
    lager:info("Connection Closed. Trying to Reconnect...~n", []),
    ecomponent_con:down(State#state.id),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    lager:info("Reconnected.~n", []),
    ecomponent_con:active(State#state.id),
    {noreply, State#state{xmppCom=XmppCom}};

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

handle_call(stop, _From, #state{xmppCom=XmppCom}=State) ->
    lager:info("Component Stopped.~n",[]),
    exmpp_session:stop(XmppCom),
    {stop, normal, ok, State};

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

-spec make_connection(JID::string(), Pass::string(), Server::string(), Port::integer()) -> {R::string(), XmppCom::pid()}.

make_connection(JID, Pass, Server, Port) -> 
    XmppCom = exmpp_session:start(),
    make_connection(XmppCom, JID, Pass, Server, Port, 20).
    
-spec make_connection(XmppCom::pid(), JID::ecomponent:jid(), Pass::string(), Server::string(), Port::integer(), Tries::integer()) -> {string(), pid()}.    

make_connection(XmppCom, JID, Pass, Server, Port, 0) -> 
    exmpp_session:stop(XmppCom),
    make_connection(JID, Pass, Server, Port);
make_connection(XmppCom, JID, Pass, Server, Port, Tries) ->
    lager:info("Connecting: ~p Tries Left~n",[Tries]),
    [User, SServer] = string:tokens(JID, "@"),
    MyJID = exmpp_jid:make(User, Server, "econ"),
    exmpp_session:auth_basic_digest(XmppCom, MyJID, Pass),
    try exmpp_session:connect_TCP(XmppCom, SServer, Port) of
        R -> 
            exmpp_session:login(XmppCom),
            lager:info("Connected.~n",[]),
            {R, XmppCom}
    catch
        Exception ->
            lager:warning("Exception: ~p~n",[Exception]),
            timer:sleep((20-Tries) * 200),
            make_connection(XmppCom, JID, Pass, Server, Port, Tries-1)
    end.

