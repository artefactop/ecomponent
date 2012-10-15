-module(ecomponent_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-type start_type() :: 
    normal | 
    {takeover, Node::node()} | 
    {failover, Node::node()}.

-type start_args() :: term().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------

-spec start() -> {ok, Pid::pid()} | {error, Reason::any()}.

start() ->
    application:start(ecomponent).
    
-spec start(StartType :: start_type(), StartArgs :: start_args() ) ->
    {ok, Pid::pid()} | {error, Reason::any()}.

start(_StartType, _StartArgs) ->
    {ok, _ProvPid} = confetti:use(ecomponent_conf, [
        {location, {"ecomponent.conf", "conf"}}
    ]),
    ecomponent_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------

-spec stop( State::any() ) -> ok.

stop(_State) ->
    io:format("Terminating: ~p~n",[_State]),
    ok.

