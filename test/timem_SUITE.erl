-module(timem_SUITE).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("common_test/include/ct.hrl").
-include("../include/ecomponent.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([insert_and_remove_test/1, expired_test/1, remove_expired_test/1]).

all() -> 
    [insert_and_remove_test, expired_test, remove_expired_test].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    timem:init(), 
    Config.
    
end_per_testcase(_, _Config) ->
    ok.

-define(ID1, <<"ec_01020304">>).
-define(ID2, <<"ec_01020305">>).
-define(ID3, <<"ec_01020306">>).
-define(ID4, <<"ec_01020307">>).
-define(ID5, <<"ec_01020308">>).

-define(P1, #matching{
        id = ?ID1, ns=?NS_PING, processor=dummy,
        tries=3, packet=""
}).
-define(P2, #matching{
        id = ?ID2, ns=?NS_PING, processor=dummy,
        tries=3, packet=""
}).
-define(P3, #matching{
        id = ?ID3, ns=?NS_PING, processor=dummy,
        tries=3, packet=""
}).
-define(P4, #matching{
        id = ?ID4, ns=?NS_PING, processor=dummy,
        tries=3, packet=""
}).
-define(P5, #matching{
        id = ?ID5, ns=?NS_PING, processor=dummy,
        tries=3, packet=""
}).

insert_and_remove_test(_Config) ->
    true = timem:insert(?ID1, ?P1),
    true = timem:insert(?ID2, ?P2),
    true = timem:insert(?ID3, ?P3),
    true = timem:insert(?ID4, ?P4),
    true = timem:insert(?ID5, ?P5),
    
    {?ID1, ?P1} = timem:remove(?ID1),
    {?ID2, ?P2} = timem:remove(?ID2),
    {?ID3, ?P3} = timem:remove(?ID3),
    {?ID4, ?P4} = timem:remove(?ID4),
    {?ID5, ?P5} = timem:remove(?ID5),
    ok.

expired_test(_Config) ->
    true = timem:insert(?ID1, ?P1),
    true = timem:insert(?ID2, ?P2),
    true = timem:insert(?ID3, ?P3),
    true = timem:insert(?ID4, ?P4),
    true = timem:insert(?ID5, ?P5),
    
    [] = timem:expired(100),
    [ ?ID1, ?ID2, ?ID3, ?ID4, ?ID5 ] = timem:expired(0),
    ok.

remove_expired_test(_Config) ->
    true = timem:insert(?ID1, ?P1),
    true = timem:insert(?ID2, ?P2),
    true = timem:insert(?ID3, ?P3),
    true = timem:insert(?ID4, ?P4),
    true = timem:insert(?ID5, ?P5),
    
    [ {?ID1, ?P1}, {?ID2, ?P2}, {?ID3, ?P3}, 
        {?ID4, ?P4}, {?ID5, ?P5} ] = timem:remove_expired(0),
    [] = timem:expired(0),
    ok.

