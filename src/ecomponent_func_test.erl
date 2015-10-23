-module(ecomponent_func_test).

-export([
    check/1,
    check/2,
    check/3,
    run/1
]).

-include("ecomponent_test.hrl").

-spec check(Test :: [string()]) -> {timeout, integer(), function()}.
%@doc Execute the suite for the list of tests passed as param. This function
%     set the environment and run all the tests in the list.
%@end
check(Test) ->
    check(Test, 120, false).

-spec check(
    Test :: [string()], 
    Timeout :: pos_integer()) -> {timeout, integer(), function()}.
%@doc Execute the suite for the list of tests passed as param. This function
%     set the environment and run all the tests in the list. As second param
%     you can configure the timeout (in seconds) for the suite of tests.
%@end
check(Test, Timeout) ->
    check(Test, Timeout, false).

-spec check(
    Test :: [string()], 
    Timeout :: pos_integer(),
    Verbose :: boolean()) -> {timeout, integer(), function()}.
%@doc Execute the suite for the list of tests passed as param. This function
%     set the environment and run all the tests in the list. As second param
%     you can configure the timeout (in seconds) for the suite of tests and
%     as third param you can set if you want to show all the logs (lager and
%     syslog) or not.
%@end
check(Tests, Timeout, Verbose) ->
    {timeout, Timeout,
        {setup,
            fun() -> start_suite(Verbose) end,
            fun(_) -> stop_suite() end,
            [ run(Test) || Test <- Tests ]
        }
    }.

start_suite(Verbose) ->
    net_kernel:start([ecomponent@localhost, shortnames]),
    timer:sleep(1000),
    mnesia:start(),
    ?meck_lager(Verbose),
    ?meck_syslog(Verbose),
    ?meck_component(),
    ?meck_metrics(),
    ?run_exmpp(),
    ok.

stop_suite() ->
    mnesia:stop(),
    unmock(),
    net_kernel:stop(),
    ok.

-spec run(Test :: string()) -> ok.
%@doc Run a single test. This function runs a single test without prepare
%     the environment.
%@end
run(Test) ->
    {" ** TEST => " ++ Test, {spawn, {setup,
    fun() ->
        parse_file(Test)
    end,
    fun(Functional) ->
        [{"start", fun() ->
            start_config_and_mocks(Functional),
            (Functional#functional.start)(self())
        end}] ++
        run_steps(Functional#functional.steps) ++
        [{"stop", fun() ->
            (Functional#functional.stop)(self()),
            stop_and_unmock(Functional)
        end}]
    end
    }}}.

start_config_and_mocks(Functional) ->
    %% TODO: add mnesia clustering options
    Config = Functional#functional.config ++ [
        {mnesia_nodes, [node()]},
        {mnesia_callback, []}
    ],
    %% TODO: launch slaves depend on cluster configuration
    ?meck_config(Config),
    mock(Functional#functional.mockups, Functional#functional.mock_opts),

    {ok, _} = ecomponent:start_link(),
    {ok, _} = ecomponent_acl:start_link(),
    JID = proplists:get_value(jid, Config, "ecomponent.bot"),
    lists:foreach(fun({Name, AtomicServerConf}) ->
        {ok, _} = ecomponent_con_worker:start_link({Name,Name}, JID,
            AtomicServerConf)
    end, proplists:get_value(servers, Config, [])),
    ok.

stop_and_unmock(Functional) ->
    ecomponent:stop(),
    ecomponent_acl:stop(),
    unmock(Functional#functional.mockups),
    ok.

-spec parse_file(Test :: string()) -> functional().
%@hidden
parse_file(Test) ->
    File = "../test/functional/" ++ Test ++ ".xml",
    {ok,XML} = file:read_file(File),
    [Parsed|_] = exmpp_xmlstream:parse_element(XML),
    Cleaned = exmpp_xml:remove_whitespaces_deeply(Parsed),
    lists:foldl(fun(Xmlel, #functional{
            steps=Steps, mockups=Mockups}=F) ->
        case parse(Xmlel) of
            [#step{}|_]=NewSteps -> 
                F#functional{steps=Steps ++ NewSteps};
            {MockOpts,[#mockup{}|_]=NewMockups} -> 
                F#functional{
                    mockups=Mockups ++ NewMockups, 
                    mock_opts=MockOpts};
            {'start-code', Code} ->
                F#functional{start=Code};
            {'stop-code', Code} ->
                F#functional{stop=Code};
            [{_,_}|_]=Config ->
                F#functional{config=Config};
            [] -> F
        end
    end, #functional{}, Cleaned#xmlel.children).

-spec mock(Mockups :: [mockup()], [atom()]) -> ok.
%@hidden
mock(Mockups,Opts) when is_list(Mockups) ->
    Modules = lists:usort([ M || #mockup{module=M} <- Mockups ]),
    lists:foreach(fun(M) ->
        case catch meck:new(M, Opts) of
            {'EXIT',{{already_started,_},_}} -> ok;
            ok -> ok;
            Error -> throw(Error)
        end
    end, Modules),
    [ mock_functions(M) || M <- Mockups ],
    ok.

-spec mock_functions(mockup()) -> ok.
%@hidden
mock_functions(#mockup{module=M,function=F,code=Code}) ->
    meck:expect(M, F, Code(self())),
    ok.

-spec unmock(Mockups :: [mockup()]) -> ok.
%@hidden
unmock(Mockups) when is_list(Mockups) ->
    Modules = lists:usort([ M || #mockup{module=M} <- Mockups ]),
    lists:foreach(fun(M) ->
        meck:unload(M)
    end, Modules),
    ok.

-spec unmock() -> ok.
%@hidden
unmock() ->
    meck:unload(),
    ok.

-spec run_steps(Steps :: [step()]) -> [term()].
%@hidden
run_steps(Steps) ->
    run_steps(Steps, undefined, []).

-spec run_steps(Steps :: [step()], PrevPacket :: exmpp_xml:xmlel(), [term()]) -> [term()].
%@hidden
run_steps([],_,Tests) ->
    Tests;

run_steps([#step{name=Name,times=T,type=code,stanza=Fun}=Step|Steps], PrevPacket, Tests) ->
    NewTests = Tests ++ [{"code: " ++ to_str(Name),
        fun() ->
            PID = self(),
            meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
                PID ! P
            end),
            Fun(PrevPacket, PID)
        end}],
    if 
        T > 1 -> run_steps([Step#step{times=T-1}|Steps], PrevPacket, NewTests);
        true -> run_steps(Steps, PrevPacket, NewTests)
    end;

run_steps([#step{name=Name,times=T,type=store,stanza=Stanza}=Step|Steps], _PrevPacket, Tests) ->
    NewTests = Tests ++ [{"store: " ++ to_str(Name), fun() ->
        true
    end}],
    %% TODO: check stanza for replace vars {{whatever}}
    if 
        T > 1 -> run_steps([Step#step{times=T-1}|Steps], Stanza, NewTests);
        true -> run_steps(Steps, Stanza, NewTests)
    end;

run_steps([#step{name=Name,times=T,type=send,stanza=Stanza,idserver=ServerID}=Step|Steps], _PrevPacket, Tests) ->
    #xmlel{name=PacketType} = Stanza,
    DefaultType = case PacketType of
        iq -> enotype;
        presence -> <<"available">>;
        message -> <<"normal">>
    end,
    TypeAttr = binary_to_list(exmpp_xml:get_attribute(Stanza, <<"type">>, DefaultType)),
    From = exmpp_xml:get_attribute(Stanza, <<"from">>, <<"bob@localhost/pc">>),
    FromJID = exmpp_jid:parse(From),
    %% TODO: check stanza for replace vars {{whatever}}
    Packet = #received_packet{
        packet_type=PacketType, type_attr=TypeAttr, raw_packet=Stanza,
        from={
            FromJID#jid.node,
            FromJID#jid.domain,
            FromJID#jid.resource}
    },
    NewTests = Tests ++ [{"send: " ++ to_str(Name), fun() ->
        PID = self(),
        meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
            PID ! P
        end),
        ?assertNotEqual(undefined, whereis(ecomponent)),
        ecomponent ! {Packet, ServerID},
        ok
    end}],
    if 
        T > 1 -> run_steps([Step#step{times=T-1}|Steps], Packet, NewTests);
        true -> run_steps(Steps, Packet, NewTests)
    end;

run_steps([#step{name=Name,times=T,type='receive',stanza=[#xmlel{}|_]=Stanzas}=Step|Steps], PrevPacket, Tests) ->
    NewTests = Tests ++ [{"receive: " ++ to_str(Name), fun() ->
        receive
            NewStanza when is_record(NewStanza, xmlel) ->
                B = NewStanza#xmlel{name = to_str(NewStanza#xmlel.name)},
                % Check if stanza is in list of stanzas
                case compare_stanzas(Stanzas, B) of
                    [] ->
                        ok;
                    RestStanzas  ->
                        run_steps([Step#step{stanza=RestStanzas}|Steps], PrevPacket, Tests),
                        ok
                end
        after Step#step.timeout ->
            ?debugFmt("TIMEOUT!! we need to receive ~p stanzas~n", [Stanzas]),
            throw(enostanza)
        end
    end}],
    if
        T > 1 -> run_steps([Step#step{times=T-1}|Steps], PrevPacket, NewTests);
        true -> run_steps(Steps, PrevPacket, NewTests)
    end;


run_steps([#step{name=Name,times=T,type='receive',stanza=#xmlel{}=Stanza}=Step|Steps], PrevPacket, Tests) ->
    NewTests = Tests ++ [{"receive: " ++ to_str(Name), fun() ->
        receive
            NewStanza when is_record(NewStanza, xmlel) ->
                A = Stanza#xmlel{name = to_str(Stanza#xmlel.name)},
                B = NewStanza#xmlel{name = to_str(NewStanza#xmlel.name)},
                compare_stanza(A, B);
            Other ->
                ?debugFmt("Received: ~n~p~n", [Other]),
                throw(ewrongstanza)
        after Step#step.timeout ->
            ?debugFmt("TIMEOUT!!! we need to receive ~p~n", [Stanza]),
            throw(enostanza)
        end
    end}],
    if
        T > 1 -> run_steps([Step#step{times=T-1}|Steps], PrevPacket, NewTests);
        true -> run_steps(Steps, PrevPacket, NewTests)
    end;

run_steps([#step{name=Name,times=T,type='receive',stanza=Fun}=Step|Steps], PrevPacket, Tests) ->
    NewTests = Tests ++ [{"code receive: " ++ to_str(Name),
        fun() -> 
            Fun(PrevPacket, self())
        end
    }],
    if
        T > 1 -> run_steps([Step#step{times=T-1}|Steps], PrevPacket, NewTests);
        true -> run_steps(Steps, PrevPacket, NewTests)
    end;

run_steps([#step{name=Name,times=T,type='quiet'}=Step|Steps], PrevPacket, Tests) ->
    NewTests = Tests ++ [{"quiet: " ++ to_str(Name), fun() ->
        receive
            Something ->
                ?debugFmt("NOT QUIET!!! ~p~n", [Something]),
                throw(Something)
        after Step#step.timeout ->
            ok
        end
    end}],
    if
        T > 1 -> run_steps([Step#step{times=T-1}|Steps], PrevPacket, NewTests);
        true -> run_steps(Steps, PrevPacket, NewTests)
    end.

-type any_xml() :: exmpp_xml:xmlel() | exmpp_xml:xmlcdata().

-spec compare_stanza(any_xml(), any_xml()) -> ok.
%@hidden
compare_stanza(#xmlcdata{cdata=Data}, #xmlcdata{cdata=Data}) -> ok;
compare_stanza(#xmlcdata{cdata=_}, #xmlcdata{cdata = <<"{{_}}">>}) -> ok;
compare_stanza(#xmlcdata{cdata = <<"{{_}}">>}, #xmlcdata{cdata=_}) -> ok;
compare_stanza(
        #xmlel{name=Name,attrs=Attrs1,children=Children1}, 
        #xmlel{name=Name,attrs=Attrs2,children=Children2}) ->
    AttrsA = lists:usort([ {N,V} || #xmlattr{name=N,value=V} <- Attrs1 ]),
    AttrsB = lists:usort([ {N,V} || #xmlattr{name=N,value=V} <- Attrs2 ]),
    case length(Attrs1) == length(Attrs2) of
        true -> ok;
        false -> throw({attrs_length, [{attrs1, Attrs1}, {attrs2, Attrs2}]})
    end,
    lists:foreach(fun
        ({{_N,<<"{{_}}">>},_}) -> ok;
        ({_,{_N,<<"{{_}}">>}}) -> ok;
        ({A,A}) -> ok;
        ({A,B}) ->
            ?debugFmt("value: ~p~n", [A]),
            ?debugFmt("expected: ~p~n", [B]),
            ?assertEqual(A,B)
    end, lists:zip(AttrsA, AttrsB)),
    Children1A = case Children1 of [C1] when is_list(C1) -> C1; C1 -> C1 end,
    Children2B = case Children2 of [C2] when is_list(C2) -> C2; C2 -> C2 end,
    ChildrenA = lists:sort([{A,undefined,undefined,to_str(B),C,D} || {A,_,_,B,C,D} <- Children1A]),
    ChildrenB = lists:sort([{A,undefined,undefined,to_str(B),C,D} || {A,_,_,B,C,D} <- Children2B]),
    case length(ChildrenA) == length(ChildrenB) of
        true -> ok;
        false -> throw({children_length, [{children1, ChildrenA}, {children2, ChildrenB}]})
    end,
    lists:foreach(fun({Child1,Child2}) ->
        compare_stanza(Child1, Child2)
    end, lists:zip(ChildrenA, ChildrenB)),
    true.

-type iq_processor_config() :: {atom(), {mod | app, atom()}}.
-type message_processor_config() :: {message_processor, {mod | app, atom()}}.
-type presence_processor_config() :: {presence_processor, {mod | app, atom()}}.
-type processors_config() :: {processors, [iq_processor_config()]}.

-type parse_processors_ret() ::
    [processors_config() | message_processor_config() | 
    presence_processor_config() | []].

-spec parse_processors([exmpp_xml:xmlel()]) -> parse_processors_ret().
%@hidden
parse_processors(Processors) ->
    lists:foldl(fun
        (#xmlel{name='iq', ns=NS}=IQ, [{processors,I},M,P]) ->
            Type = exmpp_xml:get_attribute(IQ, <<"type">>, <<"mod">>),
            Data = exmpp_xml:get_attribute(IQ, <<"data">>, <<>>),
            [{processors, [{NS, {binary_to_atom(Type, utf8), binary_to_atom(Data, utf8)}}|I]},M,P];
        (#xmlel{name='message'}=Message, [I,_,P]) ->
            Type = exmpp_xml:get_attribute(Message, <<"type">>, <<"mod">>),
            Data = exmpp_xml:get_attribute(Message, <<"data">>, <<>>),
            [I,{message_processor, {binary_to_atom(Type, utf8), binary_to_atom(Data, utf8)}},P];
        (#xmlel{name='presence'}=Presence, [I,M,_]) ->
            Type = exmpp_xml:get_attribute(Presence, <<"type">>, <<"mod">>),
            Data = exmpp_xml:get_attribute(Presence, <<"data">>, <<>>),
            [I,M,{presence_processor, {binary_to_atom(Type, utf8), binary_to_atom(Data, utf8)}}]
    end, [{processors, []},[],[]], Processors).

-type disco_info() :: {disco_info, boolean()}.
-type disco_info_features() :: {features, [binary()]}.
-type disco_info_info() :: {info, [
    {type, binary()} | {name, binary()} | {category, binary()}
]}.

-type disco_info_ret() :: 
    [disco_info() | disco_info_features() | disco_info_info()].

-spec parse_disco_info(exmpp_xml:xmlel()) ->
    disco_info_ret().
%@hidden
parse_disco_info(#xmlel{name='disco-info',children=Data}=Config) ->
    Active = binary_to_atom(exmpp_xml:get_attribute(Config, <<"active">>, <<"false">>), utf8),
    [{disco_info, Active}] ++ lists:foldl(fun
        (#xmlel{name='feature'}=Feature, [{features,Features},Identity]) ->
            Var = exmpp_xml:get_attribute(Feature, <<"var">>, <<>>),
            [{features,[Var|Features]},Identity];
        (#xmlel{name='identity'}=Identity, [Features,_]) ->
            Type = exmpp_xml:get_attribute(Identity, <<"type">>, <<"text">>),
            Category = exmpp_xml:get_attribute(Identity, <<"category">>, <<"ecomponent">>),
            Name = exmpp_xml:get_attribute(Identity, <<"name">>, <<"noname">>),
            I = {info, [{type, Type}, {name, Name}, {category, Category}]},
            [Features,I]
    end, [{features,[]},[]], Data).

-type parse_throttle_ret() :: [
    {max_per_period, pos_integer()} |
    {period_seconds, pos_integer()} |
    {whitelist, [binary()]} |
    {throttle, boolean()}
].

-spec parse_throttle(exmpp_xml:xmlel()) -> parse_throttle_ret().
%@hidden
parse_throttle(#xmlel{name='throttle'}=Config) ->
    case exmpp_xml:get_attribute(Config, <<"max-per-period">>, undefined) of
        undefined -> [];
        MaxPerPeriod -> [{max_per_period, bin_to_integer(MaxPerPeriod)}]
    end ++
    case exmpp_xml:get_attribute(Config, <<"period-seconds">>, undefined) of
        undefined -> [];
        PeriodSeconds -> [{period_seconds, bin_to_integer(PeriodSeconds)}]
    end ++
    case exmpp_xml:get_path(Config, [{element, 'whitelist'}]) of
        #xmlel{children=Items} -> 
            [{whitelist, lists:map(fun(#xmlel{name='item'}=X) ->
                exmpp_xml:get_attribute(X, <<"jid">>, <<>>)
            end, Items)}];
        _ -> 
            []
    end ++
    [{throttle, binary_to_atom(
        exmpp_xml:get_attribute(Config, <<"active">>, <<"true">>), utf8)}].

-type parse_acl_ret() :: [ {access_list_get | access_list_set, [binary()]} ].
 
-spec parse_acl(exmpp_xml:xmlel()) -> parse_acl_ret().
%@hidden
parse_acl(#xmlel{name='access-list-get'}=Config) ->
    parse_acl({Config, access_list_get});
parse_acl(#xmlel{name='access-list-set'}=Config) ->
    parse_acl({Config, access_list_set}); 
parse_acl({#xmlel{name=_Name, children=Acls}=Config, AclType}) ->
    lists:foldl(fun
        (#xmlel{name='iq', ns=NS}, [{Type,A}]) ->
            Acl = case exmpp_xml:get_path(Config, [{element, 'iq'}]) of
                      #xmlel{children=Items} ->
                          lists:map(fun(#xmlel{name='item'}=X) ->
                              exmpp_xml:get_attribute(X, <<"value">>, <<>>)
                          end, Items);
                      _ -> [] 
                  end,
            [{Type, A ++ [{NS, Acl}]}]   
    end, [{AclType, []}], Acls).

-type server_config() :: {atom(), [
    {server, string()} | {port, pos_integer()} |
    {secret, string()} | {type, active | passive}
]}.

-spec parse_servers(Servers :: exmpp_xml:xmlel()) -> [server_config()].
%@hidden
parse_servers(Servers) ->
    parse_servers(Servers, []).

-spec parse_servers(Servers :: exmpp_xml:xmlel(), 
    Config::[server_config()]) -> [server_config()].
%@hidden
parse_servers([], Config) ->
    Config;
parse_servers([#xmlel{name='server'}=Server|Servers], Config) ->
    Name = exmpp_xml:get_attribute(Server, <<"name">>, <<"default">>),
    Type = exmpp_xml:get_attribute(Server, <<"type">>, <<"active">>),
    ServerConfig = {binary_to_atom(Name, utf8), [
        {server, "localhost"},
        {port, 5555},
        {secret, "secret"},
        {type, binary_to_atom(Type, utf8)}
    ]},
    parse_servers(Servers, [ServerConfig|Config]).

-type config() ::
    {syslog_name, string()} | {jid, string()} | parse_throttle_ret() |
    parse_processors_ret() | disco_info_ret() | {servers, server_config()} |
    {request_timeout, pos_integer()}.

-spec parse(exmpp_xml:xmlel()) -> 
    {'start-code', function()} | {'stop-code', function()} | 
    [config()] | [mockup()] | [step()].
%@hidden
parse(#xmlel{name='start-code'}=Start) ->
    {'start-code', bin_to_code(general, exmpp_xml:get_cdata(Start))};

parse(#xmlel{name='stop-code'}=Stop) ->
    {'stop-code', bin_to_code(general, exmpp_xml:get_cdata(Stop))};

parse(#xmlel{name=config, children=Configs}) ->
    lists:flatmap(fun
        (#xmlel{name='syslog'}=Config) ->
            [{syslog_name, 
                binary_to_list(exmpp_xml:get_attribute(Config, <<"name">>, <<"ecomponent">>))}];
        (#xmlel{name='servers', children=Servers}) ->
            [{servers, parse_servers(Servers)}];
        (#xmlel{name='jid'}=Config) ->
            [{jid, binary_to_list(exmpp_xml:get_cdata(Config))}];
        (#xmlel{name='throttle'}=Config) ->
            parse_throttle(Config);
        (#xmlel{name='access-list-set'}=Config) ->
            parse_acl(Config);
        (#xmlel{name='access-list-get'}=Config) ->
            parse_acl(Config);
        (#xmlel{name='processors',children=Processors}) ->
            lists:flatten(parse_processors(Processors));
        (#xmlel{name='disco-info'}=Config) ->
            parse_disco_info(Config);
        (#xmlel{name='request-timeout'}=Config) ->
            [{request_timeout, bin_to_integer(exmpp_xml:get_attribute(Config, <<"value">>, <<"30">>))}]
    end, Configs);

parse(#xmlel{name=mockups, children=Mockups}=MockupsTag) ->
    MockOpts = case exmpp_xml:get_attribute(MockupsTag, <<"passthrough">>, <<"false">>) of
        <<"true">> -> [passthrough];
        _ -> []
    end ++ case exmpp_xml:get_attribute(MockupsTag, <<"strict">>, <<"false">>) of
        <<"false">> -> [non_strict];
        _ -> []
    end, 
    MockConfigs = lists:map(fun(#xmlel{}=Mockup) ->
        #mockup{
            module=binary_to_atom(exmpp_xml:get_attribute(Mockup, <<"module">>, <<>>), utf8),
            function=binary_to_atom(exmpp_xml:get_attribute(Mockup, <<"function">>, <<>>), utf8),
            code=bin_to_code(mockup, exmpp_xml:get_path(Mockup, [{element, 'code'}, cdata]))}
    end, Mockups),
    {MockOpts, MockConfigs};

parse(#xmlel{name=steps, children=Steps}) ->
    lists:map(fun
        (#xmlel{children=[]}=Step) ->
            Type = bin_to_type(exmpp_xml:get_attribute(Step, <<"type">>, <<"quiet">>)),
            #step{
                name=exmpp_xml:get_attribute(Step, <<"name">>, <<"noname">>),
                type=Type,
                times=bin_to_integer(exmpp_xml:get_attribute(Step, <<"times">>, <<"1">>)),
                timeout=bin_to_integer(exmpp_xml:get_attribute(Step, <<"timeout">>, <<"1000">>)),
                idserver=binary_to_atom(exmpp_xml:get_attribute(Step, <<"server-id">>, <<"default">>), utf8)};
        (#xmlel{children=[#xmlel{}=Child|Childs]}=Step) ->
            Type = bin_to_type(exmpp_xml:get_attribute(Step, <<"type">>, <<"send">>)),
            #step{
                name=exmpp_xml:get_attribute(Step, <<"name">>, <<"noname">>),
                type=Type,
                times=bin_to_integer(exmpp_xml:get_attribute(Step, <<"times">>, <<"1">>)),
                timeout=bin_to_integer(exmpp_xml:get_attribute(Step, <<"timeout">>, <<"1000">>)),
                stanza=case length(Childs)>0 of true -> [Child | Childs]; false -> Child end,
                idserver=binary_to_atom(exmpp_xml:get_attribute(Step, <<"server-id">>, <<"default">>), utf8)};
        (#xmlel{children=[#xmlcdata{}|_]}=Step) ->
            Type = bin_to_type(exmpp_xml:get_attribute(Step, <<"type">>, <<"code">>)),
            CodeText = exmpp_xml:get_cdata(Step),
            Fun = case Type of
            code -> 
                bin_to_code(steps, CodeText);
            'receive' -> 
                Timeout = exmpp_xml:get_attribute(Step, <<"timeout">>, <<"1000">>),
                Code = <<"receive ", CodeText/binary, 
                    " -> ok; Other -> throw(Other) after ", 
                    Timeout/binary, " -> throw(\"TIMEOUT!!!\") end">>,
                bin_to_code(steps, Code)
            end,
            Times = exmpp_xml:get_attribute(Step, <<"times">>, <<"1">>),
            #step{
                name=exmpp_xml:get_attribute(Step, <<"name">>, <<"noname">>),
                type=Type,
                times=bin_to_integer(Times),
                stanza=Fun}
    end, Steps).

-spec bin_to_code(general | steps | mockup, binary()) -> function().
%@hidden
bin_to_code(general, B) ->
    bin_to_code(<<"fun(PID) -> ", B/binary, " end.">>);

bin_to_code(steps, B) ->
    bin_to_code(<<"fun(Packet, PID) -> ", B/binary, " end.">>);

bin_to_code(mockup, B) ->
    bin_to_code(<<"fun(PID) -> fun", B/binary, " end end.">>).

-type attribute() :: {attribute, pos_integer(), record, RecordDef::term()}.

-spec get_defs(Module::atom()) -> [attribute()].
%@hidden
get_defs(Module) ->
    case ets:info(ecomponent_defs) of
    undefined ->
        code:load_file(Module),
        File = code:which(Module),
        Chunks = beam_lib:chunks(File, [abstract_code,"CInf"]),
        {ok,{_Mod,[{abstract_code,{_Version,Forms}},{"CInf",_CB}]}} = Chunks,
        Recs = lists:keysort(2, lists:ukeysort(1, 
            [{N,I,D} || {attribute,I,record,{N,_}}=D <- Forms])),
        Defs = [ Def || {_Name,_I,Def} <- Recs ],
        spawn(fun() ->
            ets:new(ecomponent_defs, [set, named_table, public]),
            ets:insert(ecomponent_defs, {defs, Defs}),
            timer:sleep(3600000)
        end),
        Defs;  
    _ ->
        [{defs,Defs}] = ets:lookup(ecomponent_defs, defs),
        Defs
    end. 

-spec bin_to_code(binary()) -> function().
%@hidden
bin_to_code(B) ->
    Code = binary_to_list(B),
    {ok, Tokens, _} = erl_scan:string(Code),
    {ok, [Form]} = erl_parse:parse_exprs(Tokens),
    Defs = get_defs(?MODULE),
    NewForms = Defs ++ [{function,1,foo,0,[{clause,1,[],[],[Form]}]}],
    [{function,L,foo,0,[{clause,L,[],[],[NewForm]}]}] = 
        erl_expand_records:module(NewForms, [strict_record_tests]),
    {value, Fun, _} = erl_eval:expr(NewForm, []),
    Fun.

-spec bin_to_integer(binary()) -> integer().
%@hidden
bin_to_integer(B) ->
    list_to_integer(binary_to_list(B)).

-spec bin_to_type(binary()) -> 'receive' | 'code' | 'store' | 'send'.
%@hidden
bin_to_type(<<"receive">>) -> 'receive';
bin_to_type(<<"code">>) -> 'code';
bin_to_type(<<"store">>) -> 'store';
bin_to_type(<<"quiet">>) -> 'quiet';
bin_to_type(<<"send">>) -> 'send';
bin_to_type(_) -> throw(invalid_step_type).

-spec to_str(Any::any()) -> string().
%@hidden
to_str(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_str(Str) when is_list(Str) -> Str;
to_str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_str(Int) when is_integer(Int) -> integer_to_list(Int);
to_str(Float) when is_float(Float) -> float_to_list(Float).

-spec compare_stanzas(any_xml(), [any_xml()]) -> [any_xml()] | [].
%@hidden
compare_stanzas(B, Stanzas) ->
    compare_stanzas(B, Stanzas, []).

-spec compare_stanzas(any_xml(), [any_xml()], list()) -> [any_xml()] | [].

compare_stanzas([], _B, _Acc) ->
    throw(eunknownstanza);
compare_stanzas([#xmlel{}=Stanza|Stanzas], B, Acc) ->
    A = Stanza#xmlel{name = to_str(Stanza#xmlel.name)},
    case catch compare_stanza(A, B) of
        {'EXIT', _} ->
            compare_stanzas(Stanzas, B, [A|Acc]);
        _           ->
            Acc ++ Stanzas
    end.
