-module(ecomponent_mnesia).

-include("../include/ecomponent.hrl").

-export([init/1, init/2]).

-spec init(Conf::proplists:proplist()) -> ok.

init(Conf) ->
    Nodes = proplists:get_value(mnesia_nodes, Conf, []),
    [ net_kernel:connect_node(X) || X <- Nodes ],
    init(Nodes, proplists:get_value(mnesia_callback, Conf, [])).

-spec init(Nodes::[atom()], [Callbacks::{M::atom(),F::atom(),A::[term()]}]) -> ok.

init([], Callbacks) ->
    mnesia:create_schema([node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies), 
    mnesia:start(),
    mnesia:create_table(monitor, [{attributes, record_info(fields, monitor)}]),
    mnesia:create_table(timem, [{attributes, record_info(fields, timem)}]),
    lists:foreach(fun({Mod,Fun,Args}) ->
        lists:foreach(fun({Table, Type, Fields}) ->
            Res = mnesia:create_table(Table, [{attributes, Fields}]),
            mnesia:change_table_copy_type(Table, node(), Type), 
            lager:info("create table (~p): ~p~n", [Res, [Table, {attributes, Fields}]])
        end, erlang:apply(Mod, Fun, Args))
    end, Callbacks),
    ok;
init([Node|Nodes], Callbacks) when Node =:= node() ->
    init(Nodes, Callbacks);
init([Node|Nodes], Callbacks) ->
    case rpc:call(Node, mnesia, system_info, [running_db_nodes]) of
        NodeList when length(NodeList) >= 1 -> 
            mnesia:create_schema([node()]),
            mnesia:start(),
            mnesia:change_config(extra_db_nodes, [Node]),
            mnesia:change_table_copy_type(schema, node(), disc_copies), 
            mnesia:add_table_copy(monitor, node(), ram_copies),
            mnesia:add_table_copy(timem, node(), ram_copies), 
            lists:foreach(fun({Mod,Fun,Args}) ->
                lists:foreach(fun({Table, Type, _Fields}) ->
                    Res = mnesia:add_table_copy(Table, node(), Type),
                    lager:info("add table copy (~p): ~p~n", [Res, [Table, node(), Type]])
                end, erlang:apply(Mod, Fun, Args))
            end, Callbacks),
            ok;
        _ ->
            init(Nodes, Callbacks)
    end.
