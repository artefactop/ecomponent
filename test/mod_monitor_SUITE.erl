-module(mod_monitor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("../include/ecomponent.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([init_test/1, accept_test/1]).

-define(WLIST_TABLE, mmwl).
-define(WHITELIST, ["white@yuilop.tv"]).
-define(WHITE, [{monitor, "white@yuilop.tv", 0, now()}]).
-define(BLACK, [{monitor, "black@yuilop.tv", 2, now()}]).
-define(EMPTY, []).

all() -> 
    [init_test, accept_test].

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
        (_Table, "white@yuilop.tv") -> ?WHITE;
        (_Table, "black@yuilop.tv") -> ?BLACK;
        (_Table, "empty@yuilop.tv") -> ?EMPTY
    end),
    Config.

end_per_suite(_Config) ->
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
    [[{"white@yuilop.tv",allowed}]] = ets:match(?WLIST_TABLE, '$1'),
    ok.

accept_test(_Config) ->
    mod_monitor:init(?WHITELIST),
    true = mod_monitor:accept("white@yuilop.tv", 0, 0),
    false = mod_monitor:accept("black@yuilop.tv", 1, 10),
    true = mod_monitor:accept("empty@yuilop.tv", 1, 10),
    ok.

