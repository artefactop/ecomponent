-module(ecomponent_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/2, start_child/3]).

%% Supervisor callbacks
-export([init/1]).

-define(MAX_RETRIES, 20).
-define(MAX_TIME, 10).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, brutal_kill, Type, [I]}).
-define(WORKER(I, Args), {I, {ecomponent_con_worker, start_link, Args}, transient, 5000, worker, [ecomponent_con_worker]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(MaxR, MaxT) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MaxR, MaxT]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ID, JID, SrvConf) ->
    supervisor:start_child(?MODULE, ?WORKER(ID, [ID, JID, SrvConf])). 

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, {{one_for_one, ?MAX_RETRIES, ?MAX_TIME}, [
        ?CHILD(ecomponent, worker)
    ]}};
init([MaxR, MaxT]) ->
    {ok, {{one_for_one, MaxR, MaxT}, [
        ?CHILD(ecomponent, worker)
    ]}}.
