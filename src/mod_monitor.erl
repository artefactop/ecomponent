%%%-------------------------------------------------------------------
%%% File	: mod_monitor.erl
%%% Author	: Thiago Camargo <barata7@gmail.com>
%%% Description : Generic Erlang/Mnesia Throttle
%%% Provides:
%%%			 * Throttle Function based on Max Requests for an Interval for an ID(node)
%%%
%%% Created : 16 Apr 2010	by Thiago Camargo <barata7@gmail.com>
%%%-------------------------------------------------------------------

-module(mod_monitor).

-export([init/0, init/1,
	accept/3]).

-define(WLIST_TABLE, mmwl).

-record(monitor, {id, counter, timestamp}).

init() -> init([]).

init(Whitelist) ->
	prepare_whitelist(Whitelist),
	mnesia:create_schema([node()]),
	application:start(mnesia),
	mnesia:create_table(monitor,
						[{attributes, record_info(fields, monitor)}]).

prepare_whitelist(L) ->
	case ets:info(?WLIST_TABLE) of
		undefined ->
			ets:new(?WLIST_TABLE, [named_table, public]);
		_ ->
			ets:delete_all_objects(?WLIST_TABLE)
	end,	
	p_w(L).

p_w([]) -> ok;
p_w([H|T]) ->
	ets:insert(?WLIST_TABLE, {H,allowed}),
	p_w(T).

is_white(K) ->
	case ets:lookup(?WLIST_TABLE, K) of
		[{_, allowed}] -> true;
		_ -> false
	end.

accept(Id, Max, Period) ->
	case is_white(Id) of
		true ->
			true;
		_ ->
			l_accept(Id, Max, Period)
	end.

l_accept(N=#monitor{}, C, Max) ->
	update_node(N, now(), C),
	if C > Max -> 
			false;
	true ->
			true
	end;

l_accept(Id, Max, Period) -> 
	N = get_node(Id),
	case N of
	{'EXIT', _Reason} ->
			false;
	_ ->
			Timestamp=N#monitor.timestamp,
		Counter=N#monitor.counter+1,
			D = to_mile(timer:now_diff(now(),Timestamp)),
	if D > Period ->
					NC = reset_counter(D, Counter, Max, Period),
					lager:info("Monitor Counter Updated: from ~p to ~p", [Counter, NC]),
		l_accept(N, NC, Max);
			true ->
					l_accept(N, Counter, Max)
			end
	end.

to_mile(T) -> T/1000000.

reset_counter(Time_delta, Counter, Max, Period) ->
	C = Counter - Max * (trunc(Time_delta/Period) + 1),
	if C < 0 -> 0;	true -> C end.

update_node(N, T, C) ->
	NN = #monitor{id=N#monitor.id, counter=C, timestamp=T},
	case mnesia:dirty_write(monitor,NN) of
		{'EXIT', _Reason} ->
				lager:error("Found no session for ~s",[id]);
		_ -> 
		NN
	end.

get_node(Id) ->
	V = mnesia:dirty_read(monitor, Id),
	case V of
	{'EXIT', _Reason} ->
		add_node(Id);
	[] -> 
		add_node(Id);
	[N|_] -> N
	end.

add_node(Node_id) ->
	N = #monitor{id=Node_id, counter=0, timestamp = now()},
	case mnesia:dirty_write(monitor,N) of
	{'EXIT', _Reason} ->
		lager:error("Found no session for ~s",[id]);
	_ -> N
	end.
	
