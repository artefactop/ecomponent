-module(timem).

-export([insert/2, remove/1, tm/1, expired/1, remove_expired/1]).

-include("../include/ecomponent.hrl").
-include_lib("stdlib/include/qlc.hrl").

-spec insert(K::binary(), V::term()) -> boolean().
    
insert(K, V) ->
    lager:debug("Inserting ~p ~p ~n", [K, V]),
    case mnesia:transaction(fun() ->
        mnesia:write(#timem{id=K, packet=V, timestamp=tm(now())})
    end) of
        {atomic, ok} -> true;
        _Reason -> 
            lager:info("Could NOT get Transaction: ~p~n",[_Reason]),
            false
    end.

-spec remove(K::binary()) -> {K::binary(), V::term()} | undefined.

remove(K) ->
    lager:debug("Removing... ~p ~n", [K]), 
    case mnesia:transaction(fun() ->
        case mnesia:read({timem, K}) of
            [] ->
                undefined;
            [R|_] ->
                lager:debug("Removed... ~p ~p~n", [K, R]),
                mnesia:delete_object(R),
                {K, R#timem.packet}
        end
    end) of
        {atomic, Record} ->
            Record
    end.

-spec expired(D::integer()) -> list(binary()).

expired(D) ->
    T = tm(now()) - D*1000000,
    {atomic, Res} = mnesia:transaction(fun() ->
        Q = qlc:q([ Id || #timem{id=Id, timestamp=Ts} <- mnesia:table(timem), Ts < T ]),
        qlc:e(Q)
    end),
    Res.

-spec remove_expired(D::integer()) -> list({K::binary(), V::term()}).

remove_expired(D) ->
    T = tm(now()) - D*1000000,
    {atomic, Res} = mnesia:transaction(fun() ->
        Q = qlc:q([ Timem || #timem{timestamp=Ts}=Timem <- mnesia:table(timem), Ts < T ]),
        Timems = qlc:e(Q),
        [ mnesia:delete_object(X) || X <- Timems ],
        [ {K,V} || #timem{id=K, packet=V} <- Timems ]
    end),
    Res.

-spec tm( T::erlang:timestamp() ) -> integer().

tm({M, S, Mc}) -> M*1000000000000 + S*1000000 + Mc.
