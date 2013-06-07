-module(timem_test).

-compile(export_all).

-include("../include/ecomponent_test.hrl").

-define(UUID, list_to_binary(uuid:to_string(uuid:uuid4()))).

-define(P(ID), #matching{
    id = ID, ns=?NS_PING, processor=dummy,
    tries=3, packet=""
}).

mnesia_callback() ->
    [].

setup_test_() ->
    {setup, 
        fun init_per_suite/0,
        fun end_per_suite/1,
        fun (Config) -> [
            insert_and_remove_test(Config),
            expired_test(Config),
            remove_expired_test(Config)
        ] end
    }.

init_per_suite() ->
    ecomponent:init_mnesia([], [{timem_test, mnesia_callback, []}]),
    [{uuids, [?UUID, ?UUID, ?UUID, ?UUID, ?UUID]}].

end_per_suite(_Config) ->
    mnesia:stop(),
    ok.

insert_and_remove_test(Config) ->
    mnesia:clear_table(timem), 
    [ID1,ID2,ID3,ID4,ID5] = proplists:get_value(uuids, Config),
    [
        ?_assert(timem:insert(ID1, ?P(ID1))),
        ?_assert(timem:insert(ID2, ?P(ID2))),
        ?_assert(timem:insert(ID3, ?P(ID3))),
        ?_assert(timem:insert(ID4, ?P(ID4))),
        ?_assert(timem:insert(ID5, ?P(ID5))),
        ?_assertEqual({ID1, ?P(ID1)}, timem:remove(ID1)),
        ?_assertEqual({ID2, ?P(ID2)}, timem:remove(ID2)),
        ?_assertEqual({ID3, ?P(ID3)}, timem:remove(ID3)),
        ?_assertEqual({ID4, ?P(ID4)}, timem:remove(ID4)),
        ?_assertEqual({ID5, ?P(ID5)}, timem:remove(ID5))
    ].

expired_test(Config) ->
    mnesia:clear_table(timem), 
    [ID1,ID2,ID3,ID4,ID5] = List = proplists:get_value(uuids, Config),
    [
        ?_assert(timem:insert(ID1, ?P(ID1))),
        ?_assert(timem:insert(ID2, ?P(ID2))),
        ?_assert(timem:insert(ID3, ?P(ID3))),
        ?_assert(timem:insert(ID4, ?P(ID4))),
        ?_assert(timem:insert(ID5, ?P(ID5))),
        ?_assertEqual([], timem:expired(100)),
        ?_assert(begin
            Expired = timem:expired(0),
            true = lists:all(fun(X) -> lists:member(X, Expired) end, List),
            5 = length(Expired),
            true
        end)
    ].

remove_expired_test(Config) ->
    mnesia:clear_table(timem), 
    [ID1,ID2,ID3,ID4,ID5] = List = proplists:get_value(uuids, Config),
    [
        ?_assert(timem:insert(ID1, ?P(ID1))),
        ?_assert(timem:insert(ID2, ?P(ID2))),
        ?_assert(timem:insert(ID3, ?P(ID3))),
        ?_assert(timem:insert(ID4, ?P(ID4))),
        ?_assert(timem:insert(ID5, ?P(ID5))),
        ?_assert(begin
            Expired = timem:remove_expired(0),
            true = lists:all(fun(X) -> lists:member({X, ?P(X)}, Expired) end, List),
            5 = length(Expired),
            [] = timem:expired(0),
            true
        end)
    ].
