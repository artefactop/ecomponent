-module(mod_monitor_test).

-compile(export_all).

-include("../include/ecomponent_test.hrl").

-define(WHITELIST, ["white@localhost"]).
-define(WHITE, [{monitor, "white@localhost", 0, now()}]).
-define(BLACK, [{monitor, "black@localhost", 2, now()}]).
-define(EMPTY, []).

setup_test_() ->
    {setup, 
        fun init_per_suite/0,
        fun end_per_suite/1,
        fun (Config) -> [
            init_test(Config),
            accept_test(Config)
        ] end
    }.

init_per_suite() ->
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
    ok.

end_per_suite(_Config) ->
    meck:unload(mnesia),
    application:stop(lager),
    application:stop(syntax_tools),
    application:stop(compiler),
    ok.

init_test(_Config) ->
    ?_assert(begin 
        mod_monitor:init(?WHITELIST),
        [[{"white@localhost",allowed}]] =:= ets:match(?WLIST_TABLE, '$1')
    end).

accept_test(_Config) ->
    ?_assert(begin
        mod_monitor:init([]),
        true = mod_monitor:accept("white@localhost", 1, 10),
        false = mod_monitor:accept("black@localhost", 1, 10),
        true = mod_monitor:accept("empty@localhost", 1, 10),
        true
    end).
