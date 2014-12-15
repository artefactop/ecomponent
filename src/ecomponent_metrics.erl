%@hidden
-module(ecomponent_metrics).

%% API
-export([
    init/0,
    init/1,
    notify_throughput_presence/2,
    notify_throughput_message/2,
    notify_throughput_iq/3, 
    set_iq_time/3, 
    notify_resp_time/1, 
    notify_dropped_presence/1,
    notify_dropped_message/1,
    notify_dropped_iq/2,

    notify/1,
    notify/2
]).

-spec init() -> ok.

init() ->
    init([]).

-spec init(Conf::proplists:proplist()) -> ok.

init(Conf) ->
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
    end,
    lists:foreach(fun(Module) ->
        [ create(Metric) || Metric <- Module:init() ]
    end, proplists:get_value(metrics_mods, Conf, [])),
    ok.

-spec notify_throughput_presence(IO::atom(), Type :: atom()) ->
    ok | {error, Name :: atom(), nonexistent_metric} |
    {error, Type :: atom(), unsupported_metric_type}.

notify_throughput_presence(IO, Type) ->
    notify(concat("presence_throughput_", concat(IO, concat("_", Type)))).

-spec notify_throughput_message(IO::atom(), Type :: atom()) ->
    ok | {error, Name :: atom(), nonexistent_metric} |
    {error, Type :: atom(), unsupported_metric_type}.

notify_throughput_message(IO, Type) ->
    notify(concat("message_throughput_", concat(IO, concat("_", Type)))).

-spec notify_throughput_iq(IO::atom(), Type :: atom(), NS :: atom()) ->
    ok | {error, Name :: atom(), nonexistent_metric} |
    {error, Type :: atom(), unsupported_metric_type}.

notify_throughput_iq(IO, Type, NS) ->
    notify(concat("iq_throughput_", concat(IO, concat("_", concat(concat(Type, "_"), NS))))).

-spec set_iq_time(Id :: binary(), Type :: atom(), NS :: atom()) -> boolean().

set_iq_time(Id, Type, NS) ->
    ets:insert(response_time, {Id, {os:timestamp(), concat(concat(Type, "_"), NS)}}).

-spec notify_resp_time(Id :: binary()) ->
    ok | {error, Name :: atom(), nonexistent_metric} |
    {error, Type :: atom(), unsupported_metric_type}.

notify_resp_time(Id) ->               
    case ets:lookup(response_time, Id) of
        [{_, {Time, Mname}}] ->
            Name = concat("response_time_", Mname),
            case ets:member(metrics, Name) of
                false ->
                    folsom_metrics:new_histogram(Name, slide, 60), %%TODO need configure time
                    ets:insert(metrics, {Name, 1});
                _ -> true
            end,
            Diff = timer:now_diff(now(), Time)/1000000, %%seconds
            ets:delete(response_time, Id),
            folsom_metrics:notify({Name, Diff});
        _ -> ok
    end.

-spec notify_dropped_presence(Type :: atom()) ->
    ok | {error, Name :: atom(), nonexistent_metric} |
    {error, Type :: atom(), unsupported_metric_type}.

notify_dropped_presence(Type) ->
    notify(concat("presence_dropped_", Type)).

-spec notify_dropped_message(Type :: atom()) ->
    ok | {error, Name :: atom(), nonexistent_metric} |
    {error, Type :: atom(), unsupported_metric_type}.

notify_dropped_message(Type) ->
    notify(concat("message_dropped_", Type)).

-spec notify_dropped_iq(Type :: atom(), NS :: atom()) ->
    ok | {error, Name :: atom(), nonexistent_metric} |
    {error, Type :: atom(), unsupported_metric_type}.

notify_dropped_iq(Type, NS) ->
    notify(concat("iq_dropped_", concat(concat(Type, "_"), NS))).

-spec concat(S :: string() | atom() | binary() | pid(), A :: string() | atom() | binary() | pid()) -> atom().

concat(S, A) when is_binary(S) ->
    concat(binary_to_list(S), A);
concat(S, A) when is_binary(A) ->
    concat(S, binary_to_list(A));
concat(S, A) when is_atom(S) ->
    concat(atom_to_list(S), A);
concat(S, A) when is_atom(A) ->
    concat(S, atom_to_list(A));
concat(S, A) when is_list(S) andalso is_list(A) ->
    erlang:list_to_atom(S ++ A);
concat(S, A) when is_pid(S) ->
    concat("PID", A);
concat(S, A) when is_pid(A) ->
    concat(S, "PID").


-spec notify(Name :: string()) -> ok.

notify(Name) ->
    notify(Name, 1).

-spec notify(Name :: string(), Value :: integer()) -> ok.

notify(Name, Value) ->
    create(Name),
    folsom_metrics:notify({Name, Value}).

-spec create(Metric :: atom()) -> true.

create(Metric) ->
    case ets:member(metrics, Metric) of 
        false ->
            folsom_metrics:new_spiral(Metric), %% last 60 sec
            ets:insert(metrics, {Metric, 1});
        _ -> true
    end.
