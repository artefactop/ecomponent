%@hidden
-module(ecomponent_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(MAX_RETRIES, 20).
-define(MAX_TIME, 10).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, brutal_kill, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(MaxR, MaxT) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MaxR, MaxT]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
    {ok, {{one_for_all, ?MAX_RETRIES, ?MAX_TIME}, [
        ?CHILD(ecomponent_con_sup, supervisor),
        ?CHILD(ecomponent, worker),
        ?CHILD(ecomponent_acl, worker)
    ]}};
init([MaxR, MaxT]) ->
    {ok, {{one_for_all, MaxR, MaxT}, [
        ?CHILD(ecomponent_con_sup, supervisor),
        ?CHILD(ecomponent, worker)
    ]}}.
