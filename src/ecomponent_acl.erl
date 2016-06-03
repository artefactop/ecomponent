-module(ecomponent_acl).
-compile([warnings_as_errors]).

-behaviour(gen_server).

-include("ecomponent_internal.hrl").

%% API
-export([
    access_list_set/2, access_list_get/2]).

%% gen_server callbacks
-export([
    start_link/0, stop/0, init/1, handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3]).

-record(rstate, {
    accessListSet = [] :: accesslist(),
    accessListGet = [] :: accesslist()
}).


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

-spec access_list_set(NS::atom(), Jid::jid()) -> boolean().
%@doc check if is allowed for this JID send an IQ SET with this NameSpace.
access_list_set(NS, Jid) ->
    gen_server:call(?MODULE, {access_list_set, NS, Jid}).

-spec access_list_get(NS::atom(), Jid::jid()) -> boolean().
%@doc check if is allowed for this JID send an IQ GET with this NameSpace.
access_list_get(NS, Jid) ->
    gen_server:call(?MODULE, {access_list_get, NS, Jid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init( Args :: [] ) -> 
    {ok, State :: #rstate{}}.
%@hidden
init([]) ->
    Conf = application:get_all_env(ecomponent),
    {ok, #rstate{
        accessListSet = proplists:get_value(access_list_set, Conf, []),
        accessListGet = proplists:get_value(access_list_get, Conf, [])
    }}.

-spec handle_info(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}}.
%@hidden
handle_info(Record, State) -> 
    lager:info("Unknown Info Request: ~p~n", [Record]),
    {noreply, State}.

-spec handle_cast(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}}.
%@hidden
handle_cast(_Msg, State) ->
    lager:info("Received: ~p~n", [_Msg]), 
    {noreply, State}.


-spec handle_call(Msg::any(), From::{pid(),_}, State::#state{}) ->
    {reply, Reply::any(), State::#state{}} |
    {stop, Reason::any(), Reply::any(), State::#state{}}.
%@hidden
handle_call({access_list_set, NS, Jid} = Info, _From, State) ->
    lager:debug("Received Call: ~p~n", [Info]),
    {reply, is_allowed(set, NS, Jid, State), State};

handle_call({access_list_get, NS, Jid} = Info, _From, State) ->
    lager:debug("Received Call: ~p~n", [Info]),
    {reply, is_allowed(get, NS, Jid, State), State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Info, _From, State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, ok, State}.


-spec terminate(Reason::any(), State::#state{}) -> ok.
%@hidden
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn::string(), State::#state{}, Extra::any()) ->
    {ok, State::#state{}}.
%@hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec is_allowed( (set | get | error | result), NS::atom(), JID::jid(), State::#rstate{}) -> boolean().
%@doc Check if is allowed the request (based on ACL lists).
%@end
is_allowed(Type, NS, {Node,Domain,Res}, State) when is_list(Domain) ->
    is_allowed(Type, NS, {Node,list_to_binary(Domain),Res}, State);
is_allowed(set, NS, {_, Domain, _}, #rstate{accessListSet=As}) ->
    is_allowed(NS, Domain, As);
is_allowed(get, NS, {_, Domain, _}, #rstate{accessListGet=Ag}) ->
    is_allowed(NS, Domain, Ag).

-spec is_allowed( NS::atom(), Domain::string(), PList::list(binary()) ) -> boolean().
%@hidden
is_allowed(NS, Domain, PList) ->
    case proplists:get_value(NS, PList) of
        undefined ->
            true;
        List ->
            lists:member(Domain, List)
    end.
