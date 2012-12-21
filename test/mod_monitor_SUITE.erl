-module(mod_monitor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("../include/ecomponent.hrl").

-export([all/0, suite/0]).
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([init_test/1, accept_test/1]).

-define(WHITELIST, ["white@localhost"]).
-define(WHITE, [{monitor, "white@localhost", 0, now()}]).
-define(BLACK, [{monitor, "black@localhost", 2, now()}]).
-define(EMPTY, []).

all() -> 
    [init_test, accept_test].

suite() ->
    [{ct_hooks,[{cth_junit, [{path, "junit_mod_monitor.xml"}]}]},{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    
    meck:new(mnesia, [no_link]),
    meck:expect(mnesia, start, fun() -> ok end),
    meck:expect(mnesia, create_schema, fun(_Params) -> ok end),
    meck:expect(mnesia, create_table, fun(_Name, _Opts) -> ok end),
    meck:expect(mnesia, dirty_write, fun(_Table, _Data) -> ok end),
    meck:expect(mnesia, dirty_read, fun
        (_Table, "white@localhost") -> ?WHITE;
        (_Table, "black@localhost") -> ?BLACK;
        (_Table, "empty@localhost") -> ?EMPTY
    end),
    Config.

end_per_suite(_Config) ->
    meck:unload(mnesia),
    application:stop(lager),
    application:stop(syntax_tools),
    application:stop(compiler),
    ok.

init_per_testcase(_, Config) ->
    Config.
    
end_per_testcase(_, _Config) ->
    ok.

init_test(_Config) ->
    mod_monitor:init(?WHITELIST),
    [[{"white@localhost",allowed}]] = ets:match(?WLIST_TABLE, '$1'),
    ok.

accept_test(_Config) ->
    mod_monitor:init(?WHITELIST),
    true = mod_monitor:accept("white@localhost", 0, 0),
    false = mod_monitor:accept("black@localhost", 1, 10),
    true = mod_monitor:accept("empty@localhost", 1, 10),
    ok.

