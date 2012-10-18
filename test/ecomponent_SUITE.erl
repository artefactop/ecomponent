-module(ecomponent_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([config_test/1]).

all() -> 
    [config_test].

init_per_suite(Config) ->
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    
    meck:new(syslog, [no_link]),
    meck:expect(syslog, open, fun(_Name, _Opts, _Facility) -> ok end),
    
    meck:new(confetti, [no_link]),
    meck:expect(confetti, fetch, fun(_) -> [
        {syslog_name, "ecomponent" },
        {jid, "ecomponent.test" },
        {server, "localhost" },
        {port, 8899},
        {pass, "secret"},
        {whitelist, [] }, %% throttle whitelist
        {access_list_get, []},
        {access_list_set, [
            {'com.yuilop.push/message', [<<"sendpush.localhost">>]},
            {'com.yuilop.push/jingle-initiate', [<<"sendpush.localhost">>]},
            {'com.yuilop.push/jingle-terminate', [<<"sendpush.localhost">>]},
            {'com.yuilop.push/multimedia/files', [<<"sendpush.localhost">>]},
            {'com.yuilop.push/multimedia/location', [<<"sendpush.localhost">>]},
            {'com.yuilop.push/contacts', [<<"sendpush.localhost">>]}
        ]},
        {max_per_period, 15},
        {period_seconds, 8},
        {processors, [
            {default, {mod, dummy}}
        ]}
    ] end),
    
    meck:new(exmpp_component, [no_link]),
    meck:expect(exmpp_component, start, fun() -> self() end),
    meck:expect(exmpp_component, stop, fun(_) -> ok end),
    meck:expect(exmpp_component, auth, fun(_Pid, _JID, _Pass) -> ok end),
    meck:expect(exmpp_component, connect, fun(_Pid, _Server, _Port) -> "1234" end),
    meck:expect(exmpp_component, handshake, fun(_Pid) -> ok end),
    
    Config.

end_per_suite(_Config) ->
    meck:unload(syslog),
    meck:unload(confetti),
    meck:unload(exmpp_component),
    application:stop(lager),
    application:stop(syntax_tools),
    application:stop(compiler),
    ok.

init_per_testcase(_, Config) ->
    Config.
    
end_per_testcase(_, _Config) ->
    ok.

config_test(_Config) ->
    {ok, State} = ecomponent:configure(),
    State = {state, 
        self(), "ecomponent.test", <<"ec_00000000">>, "secret",
        "localhost", 8899, [], 15, 8, [{default, {mod, dummy}}],
        3, 100, 10, [
            {'com.yuilop.push/message', [<<"sendpush.localhost">>]},
            {'com.yuilop.push/jingle-initiate', [<<"sendpush.localhost">>]},
            {'com.yuilop.push/jingle-terminate', [<<"sendpush.localhost">>]},
            {'com.yuilop.push/multimedia/files', [<<"sendpush.localhost">>]},
            {'com.yuilop.push/multimedia/location', [<<"sendpush.localhost">>]},
            {'com.yuilop.push/contacts', [<<"sendpush.localhost">>]}
        ], [], local7, "ecomponent"},
    ok.

