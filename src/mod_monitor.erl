%%-------------------------------------------------------------------
%% File        : mod_monitor.erl
%% Author      : Thiago Camargo <barata7@gmail.com>
%%             : Manuel Rubio <manuel@yuilop.com>
%% Description : Generic Erlang/Mnesia Throttle
%% Provides:
%%   * Throttle Function based on Max Requests for an Interval for
%%     an ID(node)
%%
%% Created : 16 Apr 2010 by Thiago Camargo <barata7@gmail.com>
%% Updated : 21 Dec 2012 by Manuel Rubio <manuel@yuilop.com>
%%-------------------------------------------------------------------
-module(mod_monitor).

-export([init/1, accept/3]).

-include("ecomponent.hrl").

-spec init( Whitelist :: list(binary()) ) -> ok.
%@doc Init the monitor. Adds the JIDs to the whitelist.
%@end
init(Whitelist) ->
    prepare_whitelist(Whitelist).

-spec prepare_whitelist( L :: list(binary()) ) -> ok.
%@doc Prepare the whitelist. Adds the elements to the whitelist.
prepare_whitelist(L) ->
    case ets:info(?WLIST_TABLE) of
        undefined ->
            ets:new(?WLIST_TABLE, [named_table, public]);
        _ ->
            ets:delete_all_objects(?WLIST_TABLE)
    end,
    [ ets:insert(?WLIST_TABLE, {H,allowed}) || H <- L ],
    ok.

-spec is_white( K :: string() ) -> boolean().
%@doc Check if the param is whitelisted. Check if the passed param
%     is in the whitelist.
%@end
is_white(K) ->
    case ets:info(?WLIST_TABLE) of
        undefined ->
            true;
        _ ->
            [{K, allowed}] =:= ets:lookup(?WLIST_TABLE, K)
    end.

-spec accept( Id :: string(), Max :: integer(), Period :: integer() ) -> boolean().
%@doc Check if the packet can be accepted. It depends if ID is whitelisted,
%     and the Max packets can be accepted in the Period seconds.
%@end
accept(Id, Max, Period) ->
    case is_white(Id) of
        true ->
            true;
        false ->
            N = get_node(Id),
            Counter = N#monitor.counter+1,
            D = (timer:now_diff(os:timestamp(), N#monitor.timestamp)) / 1000000,
            if 
                D > Period ->
                    NC = case Counter - Max * trunc(D / Period + 1) of
                        C when C < 0 -> 0;
                        C -> C
                    end,
                    lager:debug("monitor updated id=<~p>; from ~p to ~p", [Id, Counter, NC]),
                    mnesia:dirty_write(monitor, N#monitor{counter=NC, timestamp=os:timestamp()}),
                    NC =< Max;
                true ->
                    mnesia:dirty_write(monitor, N#monitor{counter=Counter, timestamp=os:timestamp()}),
                    Counter =< Max
            end
    end.

-spec get_node( Id :: string() ) -> #monitor{}.
%@doc Get node information about monitor. If the node doesn't exists 
%     will be created.
%@end
get_node(Id) ->
    case catch mnesia:dirty_read(monitor, Id) of
        {'EXIT', _Reason} ->
            add_node(Id);
        [] -> 
            add_node(Id);
        [N|_] -> 
            N
    end.

-spec add_node( Id :: string() ) -> #monitor{}.
%@doc Add a node in the monitor. If the node exists will be reset.
%@end
add_node(Id) ->
    N = #monitor{id=Id, counter=0, timestamp=os:timestamp()},
    mnesia:dirty_write(monitor, N),
    N.
