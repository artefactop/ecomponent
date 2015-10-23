-module(processor_fake).
-compile(export_all).

check_pool(_PrevPacket,_PID) ->
    default = gen_server:call(ecomponent_con, get_pool),
    default = gen_server:call(ecomponent_con, get_pool),
    true = ecomponent_con:is_active(default),
    ok.
