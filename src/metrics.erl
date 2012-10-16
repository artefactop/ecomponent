-module(metrics).

%% API
-export([init/0, set_iq_id/1, get_iq_id/1]).

init(NSList) ->
    Sec = 60,
    generate_throughput_ns(NSList, Sec)
    generate_response_time_ns(NSList, Sec),
    case ets:info(response_time) of
        undefined ->
            ets:new(response_time, [named_table, public]);
        _ ->
            ets:delete_all_objects(response_time)
    end.

generate_throughput_ns()([], Sec) ->
    ok;
generate_throughput_ns([H|T], Sec) ->
    folsom_metrics:new_histogram(concat("throughput_", NS), slide, Sec),
    generate_throughput_ns(T).

generate_response_time_ns([], Sec) ->
    ok;
generate_response_time_ns([H|T], Sec) ->
    folsom_metrics:new_histogram(concat("response_time_", NS), slide, Sec),
    generate_response_time_ns(T).    

set_iq_time(Id) ->
    ets:insert(response_time, {Id, now()}).

iq_time_diff(Id, NS) ->
    case ets:lookup(response_time, Id) of
        [{_, Time}] ->
            Diff = timer:now_diff(now(), Time)/1000000,
            folsom_metrics:notify({concat("response_time_", NS), Diff});
        _ -> ok
    end.

concat(S::String, A:Atom) ->
    erlang:list_to_atom(S ++ erlang:atom_to_list(A)).