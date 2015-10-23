-module(timem_fake).
-compile(export_all).

insert(_,_) ->
    ok.

remove(PID, ID) when is_tuple(ID) ->
    PID ! ID,
    server_two.

remove_expired(_) ->
    [].
