-module(timem_SUITE).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("common_test/include/ct.hrl").
-include("../include/ecomponent.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([insert_and_remove_test/1, expired_test/1, remove_expired_test/1]).

-define(UUID, list_to_binary(uuid:to_string(uuid:uuid4()))).

-define(P(ID), #matching{
        id = ID, ns=?NS_PING, processor=dummy,
        tries=3, packet=""
}).

all() -> 
    [insert_and_remove_test, expired_test, remove_expired_test].

init_per_suite(_Config) ->
    [{uuids, [?UUID, ?UUID, ?UUID, ?UUID, ?UUID]}].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    timem:init(), 
    Config.
    
end_per_testcase(_, _Config) ->
    ok.

insert_and_remove_test(Config) ->
    [ID1,ID2,ID3,ID4,ID5] = proplists:get_value(uuids, Config),
    true = timem:insert(ID1, ?P(ID1)),
    true = timem:insert(ID2, ?P(ID2)),
    true = timem:insert(ID3, ?P(ID3)),
    true = timem:insert(ID4, ?P(ID4)),
    true = timem:insert(ID5, ?P(ID5)),
    
    {ID1, ?P(ID1)} = timem:remove(ID1),
    {ID2, ?P(ID2)} = timem:remove(ID2),
    {ID3, ?P(ID3)} = timem:remove(ID3),
    {ID4, ?P(ID4)} = timem:remove(ID4),
    {ID5, ?P(ID5)} = timem:remove(ID5),
    ok.

expired_test(Config) ->
    [ID1,ID2,ID3,ID4,ID5] = proplists:get_value(uuids, Config),
    true = timem:insert(ID1, ?P(ID1)),
    true = timem:insert(ID2, ?P(ID2)),
    true = timem:insert(ID3, ?P(ID3)),
    true = timem:insert(ID4, ?P(ID4)),
    true = timem:insert(ID5, ?P(ID5)),
    
    [] = timem:expired(100),
    [ ID1, ID2, ID3, ID4, ID5 ] = timem:expired(0),
    ok.

remove_expired_test(Config) ->
    [ID1,ID2,ID3,ID4,ID5] = proplists:get_value(uuids, Config),
    true = timem:insert(ID1, ?P(ID1)),
    true = timem:insert(ID2, ?P(ID2)),
    true = timem:insert(ID3, ?P(ID3)),
    true = timem:insert(ID4, ?P(ID4)),
    true = timem:insert(ID5, ?P(ID5)),
    
    [ {ID1, ?P(ID1)}, {ID2, ?P(ID2)}, {ID3, ?P(ID3)}, 
        {ID4, ?P(ID4)}, {ID5, ?P(ID5)} ] = timem:remove_expired(0),
    [] = timem:expired(0),
    ok.

