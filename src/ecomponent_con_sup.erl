%@hidden
-module(ecomponent_con_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ID, JID, SrvConf) ->
    supervisor:start_child(?MODULE, [ID, JID, SrvConf]).

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
    {ok, {{simple_one_for_one, 100, 10}, [
        {ecomponent_con_workers, 
            {ecomponent_con_worker, start_link, []}, 
            transient, 
            brutal_kill, 
            worker, 
            [ecomponent_con_worker]}
    ]}}.
