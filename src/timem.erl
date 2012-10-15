-module(timem).

-export([init/0, insert/2, remove/1, tm/1, expired/1, remove_expired/1]).

-spec init() -> ok.

init() ->
    prepare_table(timem_kv, [named_table, public] ),
    prepare_table(timem_tks, [named_table, ordered_set, public]),
    ok.

-spec prepare_table(Name::atom(), Props::term()) -> ok.

prepare_table(Name, Props) ->
        case ets:info(Name) of
                undefined ->
                        ets:new(Name, Props);
                _ ->
                        ets:delete_all_objects(Name)
        end,
        ok.

-spec insert(K::term(), V::term()) -> boolean().
    
insert(K, V) ->
    T = tm(now()),
    case ets:lookup(timem_tks, T) of
        [] ->
            RT = ets:insert(timem_tks, {T, [K]});
        [{_, L}] ->
            case lists:any(fun(X) -> K == X end, L) of
                true ->
                    LL = L;
                _ -> 
                    LL = [K|L]
            end,
            RT = ets:insert(timem_tks, {T, LL});
        _ ->
            RT = false
    end,
    case RT of
        true ->
            ets:insert(timem_kv, {K, {V, T}});
        _ ->
            false
    end.

-spec remove(K::term()) -> {K::term(), V::term()} | undefined.

remove(K) ->
    case ets:lookup(timem_kv, K) of
        [{K, {V, T}}] ->
            ets:delete(timem_kv, K),
            case ets:lookup(timem_tks, T) of
                [{T, L}] ->
                    R = lists:delete(K, L),
                    ets:insert(timem_tks, {T, R}),
                    case L of
                        [_|_R] ->
                            ets:delete(timem_tks, T);
                        _ -> ok
                    end,
                    true;
                _ ->
                    ok
            end,
            {K, V};
        _ ->
            undefined
    end.

-spec expired(D::integer()) -> ok.

expired(D) ->
    T = tm(now()) - D*1000000,
    [ X || X <- ets:select(timem_tks, [{{'$1','$2'},[{'<','$1',T}],['$2']}]) ],
    ok.

-spec remove_expired(D::integer()) -> ok.

remove_expired(D) ->
    [ remove(K) || K <- expired(D) ],
    ok.

-spec tm( T::erlang:timestamp() ) -> integer().

tm({M, S, Mc}) -> M*1000000000000 + S*1000000 + Mc.

