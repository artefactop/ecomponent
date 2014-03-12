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
%@doc Insert an element in the database with a timestamp. This information
%     will be useful to do the expiration or resend.
%@end
insert(K, V) ->
    mnesia:dirty_write(#timem{id=K, packet=V, timestamp=tm(os:timestamp())}) =:= ok.

-spec remove(K::binary()) -> timem() | undefined.
%@doc Remove an element from the database.
%@end
remove(K) ->
    case mnesia:dirty_read({timem, K}) of
        [] ->
            undefined;
        [R|_] ->
            mnesia:dirty_delete_object(R),
            {K, R#timem.packet}
    end.

-spec expired(D::integer()) -> [binary()].
%@doc Request all the expired elements.
%@end
expired(D) ->
    T = tm(os:timestamp()) - D*1000000,
    {atomic, Res} = mnesia:transaction(fun() ->
        Q = qlc:q([ Id || #timem{id=Id, timestamp=Ts} <- mnesia:table(timem), Ts < T ]),
        qlc:e(Q)
    end),
    Res.

-spec remove_expired(D::integer()) -> [timem()].
%@doc Request and remove all the expired elements.
%@end
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
%@hidden
tm({M, S, Mc}) -> M*1000000000000 + S*1000000 + Mc.
