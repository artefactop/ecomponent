-module(metrics).

%% API
-export([init/0, notify_throughput/1, set_iq_time/1, notify_resp_time/2]).

init() ->
    Sec = 60,
    case ets:info(metrics) of
        undefined ->
            ets:new(metrics, [named_table, public]);
        _ ->
            ets:delete_all_objects(metrics)
    end,
    case ets:info(response_time) of
        undefined ->
            ets:new(response_time, [named_table, public]);
        _ ->
            ets:delete_all_objects(response_time)
    end.

notify_throughput(NS) ->
    Name = concat("throughput_", NS),
    case ets:member(metrics, Name) of
        false ->
            folsom_metrics:new_histogram(Name, slide, 60), %%TODO need configure time
            ets:insert(metrics, Name);
        _ -> ok
    end,
    folsom_metrics:notify({Name, 1}).

set_iq_time(Id) ->
    ets:insert(response_time, {Id, now()}).

notify_resp_time(Id, NS) ->
    Name = concat("response_time_", NS),
    case ets:member(metrics, Name) of
        false ->
            folsom_metrics:new_histogram(Name, slide, 60), %%TODO need configure time
            ets:insert(metrics, Name);
        _ -> ok
    end,                
    case ets:lookup(response_time, Id) of
        [{_, Time}] ->
            Diff = timer:now_diff(now(), Time)/1000000, %%seconds
            folsom_metrics:notify({Name, Diff});
        _ -> ok
    end.

-spec concat(S :: string(), A :: atom()) -> atom().
concat(S, A) ->
    erlang:list_to_atom(S ++ erlang:atom_to_list(A)).
