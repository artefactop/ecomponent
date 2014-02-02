-module(timem).

-export([
    insert/2, 
    remove/1, 
    expired/1, 
    remove_expired/1
]).

-include_lib("stdlib/include/qlc.hrl").
-include("ecomponent.hrl").

-type timem() :: {K::binary(), V::term()}.

-spec insert(K::binary(), V::term()) -> boolean().
    
insert(K, V) ->
    case mnesia:transaction(fun() ->
        mnesia:write(#timem{id=K, packet=V, timestamp=tm(os:timestamp())})
    end) of
        {atomic, ok} -> true;
        _ -> false
    end.

-spec remove(K::binary()) -> timem() | undefined.

remove(K) ->
    case mnesia:transaction(fun() ->
        case mnesia:read({timem, K}) of
            [] ->
                undefined;
            [R|_] ->
                mnesia:delete_object(R),
                {K, R#timem.packet}
        end
    end) of
        {atomic, Record} ->
            Record
    end.

-spec expired(D::integer()) -> [binary()].

expired(D) ->
    T = tm(os:timestamp()) - D*1000000,
    {atomic, Res} = mnesia:transaction(fun() ->
        Q = qlc:q([ Id || #timem{id=Id, timestamp=Ts} <- mnesia:table(timem), Ts < T ]),
        qlc:e(Q)
    end),
    Res.

-spec remove_expired(D::integer()) -> [timem()].

remove_expired(D) ->
    T = tm(os:timestamp()) - D*1000000,
    {atomic, Res} = mnesia:transaction(fun() ->
        Q = qlc:q([ Timem || #timem{timestamp=Ts}=Timem <- mnesia:table(timem), Ts < T ]),
        Timems = qlc:e(Q),
        [ mnesia:delete_object(X) || X <- Timems ],
        [ {K,V} || #timem{id=K, packet=V} <- Timems ]
    end),
    Res.

-spec tm( T::erlang:timestamp() ) -> integer().

tm({M, S, Mc}) -> M*1000000000000 + S*1000000 + Mc.
