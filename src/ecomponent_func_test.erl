-module(ecomponent_func_test).

-export([
    check/1,
    check/2,
    run/1
]).

-include("ecomponent_test.hrl").

check(Test) ->
    check(Test, 120).

check(Tests, Timeout) when is_list(Tests) ->
    {timeout, Timeout, ?_assert(begin
        net_kernel:start([ecomponent@localhost, shortnames]),
        timer:sleep(1000),
        mnesia:start(),
        ?meck_lager(),
        ?meck_syslog(),
        ?meck_component(),
        ?meck_metrics(),
        ?run_exmpp(),
        [ run(Test) || Test <- Tests ],
        mnesia:stop(),
        unmock(),
        true
    end)};

check(Test, Timeout) ->
    check([Test], Timeout).

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
            [#mockup{}|_]=NewMockups -> 
                F#functional{mockups=Mockups ++ NewMockups};
            {'start-code', Code} ->
                F#functional{start=Code};
            {'stop-code', Code} ->
                F#functional{stop=Code};
            [{_,_}|_]=Config ->
                F#functional{config=Config};
            [] -> F
        end
    end, #functional{}, Cleaned#xmlel.children).

run(Test) ->
    ?debugFmt("~n~nCheck Functional Test: ~p~n", [Test]),
    MainPID = self(),
    {ProcessPID, ProcessRef} = spawn_monitor(fun() ->
        Functional = parse_file(Test),
        %% TODO: add server options
        AtomicServerConf = [
            {server, "localhost" },
            {port, 8899},
            {pass, "secret"}
        ],
        ServerConfig = [
            {servers, [
                {default, AtomicServerConf}
            ]}
        ],
        %% TODO: add mnesia clustering options
        Config = Functional#functional.config ++ ServerConfig ++ [{mnesia_nodes, [node()]}],
        %% TODO: launch slaves depend on cluster configuration
        ?debugFmt("config = ~p~n", [Config]),
        ?meck_config(Config),
        PID = self(),
        meck:expect(exmpp_component, send_packet, fun(_XmppCom, P) ->
            ?debugFmt("Send info ~p to ~p~n", [PID, P]),
            PID ! P
        end),
        mock(Functional#functional.mockups),

        {ok, _} = ecomponent:start_link(),
        JID = proplists:get_value(jid, Config, "ecomponent.bot"),
        %% TODO: depends on config, launch one or more workers
        {ok, _} = ecomponent_con_worker:start_link(default, JID, AtomicServerConf),

        (Functional#functional.start)(),

        run_steps(Functional#functional.steps),

        (Functional#functional.stop)(),

        ecomponent:stop(),
        meck:unload(application),
        unmock(Functional#functional.mockups)
    end),
    receive {'DOWN',ProcessRef,process,ProcessPID,normal} -> ok end,
    ok.

mock(Mockups) when is_list(Mockups) ->
    Modules = lists:usort([ M || #mockup{module=M} <- Mockups ]),
    lists:foreach(fun(M) ->
        ?debugFmt("meck:new(~p).~n", [M]),
        meck:new(M)
    end, Modules),
    [ mock_functions(M) || M <- Mockups ],
    ok.

mock_functions(#mockup{module=M,function=F,code=Code}) ->
    ?debugFmt("mockup ~p:~p~n", [M,F]),
    meck:expect(M, F, Code),
    ok.

unmock(Mockups) when is_list(Mockups) ->
    Modules = lists:usort([ M || #mockup{module=M} <- Mockups ]),
    lists:foreach(fun(M) ->
        meck:unload(M)
    end, Modules),
    ok.

unmock() ->
    meck:unload(),
    ok.

run_steps(Steps) ->
    run_steps(Steps, undefined).

run_steps([],_) ->
    ok;

run_steps([#step{name=Name,times=T,type=code,stanza=Fun}=Step|Steps], PrevPacket) ->
    ?debugFmt("STEP (code): ~s~n", [Name]),
    Fun(PrevPacket, self()),
    if 
        T > 1 -> run_steps([Step#step{times=T-1}|Steps], PrevPacket);
        true -> run_steps(Steps, PrevPacket)
    end;

run_steps([#step{name=Name,times=T,type=store,stanza=Stanza}=Step|Steps], _PrevPacket) ->
    ?debugFmt("STEP (store): ~s~n", [Name]),
    ?debugFmt("Store: ~n~s~n", [exmpp_xml:document_to_binary(Stanza)]),
    %% TODO: check stanza for replace vars {{whatever}}
    if 
        T > 1 -> run_steps([Step#step{times=T-1}|Steps], Stanza);
        true -> run_steps(Steps, Stanza)
    end;

run_steps([#step{name=Name,times=T,type=send,stanza=Stanza}=Step|Steps], _PrevPacket) ->
    ?debugFmt("STEP (send): ~s~n", [Name]),
    ?debugFmt("Send: ~n~s~n", [exmpp_xml:document_to_binary(Stanza)]),
    #xmlel{name=PacketType} = Stanza,
    TypeAttr = binary_to_list(exmpp_xml:get_attribute(Stanza, <<"type">>, <<"normal">>)),
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
    ?assertNotEqual(undefined, whereis(ecomponent)),
    ecomponent ! {Packet, default},
    if 
        T > 1 -> run_steps([Step#step{times=T-1}|Steps], Packet);
        true -> run_steps(Steps, Packet)
    end;

run_steps([#step{name=Name,times=T,type='receive',stanza=Stanza}=Step|Steps], _PrevPacket) ->
    ?debugFmt("STEP (receive): ~s~n", [Name]),
    ?debugFmt("Waiting for: ~n~s~n", [exmpp_xml:document_to_binary(Stanza)]),
    receive
        NewStanza -> 
            compare_stanza(Stanza, NewStanza)
    after Step#step.timeout ->
        ?debugFmt("TIMEOUT!!! we need to receive ~p~n", [Stanza]),
        throw(enostanza)
    end,
    if 
        T > 1 -> run_steps([Step#step{times=T-1}|Steps], Stanza);
        true -> run_steps(Steps, Stanza)
    end.

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
    ChildrenA = lists:sort([{A,undefined,undefined,B,C,D} || {A,_,_,B,C,D} <- Children1]),
    ChildrenB = lists:sort([{A,undefined,undefined,B,C,D} || {A,_,_,B,C,D} <- Children2]),
    case length(ChildrenA) == length(ChildrenB) of
        true -> ok;
        false -> throw({children_length, [{children1, ChildrenA}, {children2, ChildrenB}]})
    end,
    lists:foreach(fun({Child1,Child2}) ->
        compare_stanza(Child1, Child2)
    end, lists:zip(ChildrenA, ChildrenB)),
    true.

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

parse(#xmlel{name='start-code'}=Start) ->
    {'start-code', bin_to_code(general, exmpp_xml:get_cdata(Start))};

parse(#xmlel{name='stop-code'}=Stop) ->
    {'stop-code', bin_to_code(general, exmpp_xml:get_cdata(Stop))};

parse(#xmlel{name=config, children=Configs}) ->
    lists:flatmap(fun
        (#xmlel{name='syslog'}=Config) ->
            [{syslog_name, 
                binary_to_list(exmpp_xml:get_attribute(Config, <<"name">>, <<"ecomponent">>))}];
        (#xmlel{name='jid'}=Config) ->
            [{jid, binary_to_list(exmpp_xml:get_cdata(Config))}];
        (#xmlel{name='throttle'}=Config) ->
            [{throttle, 
                binary_to_atom(exmpp_xml:get_attribute(Config, <<"active">>, <<"true">>), utf8)}];
        (#xmlel{name='processors',children=Processors}) ->
            lists:flatten(parse_processors(Processors));
        (#xmlel{name='disco-info'}=Config) ->
            parse_disco_info(Config);
        (#xmlel{name='request-timeout'}=Config) ->
            [{request_timeout, bin_to_integer(exmpp_xml:get_attribute(Config, <<"value">>, <<"30">>))}]
    end, Configs);

parse(#xmlel{name=mockups, children=Mockups}) ->
    lists:map(fun(#xmlel{}=Mockup) ->
        #mockup{
            module=binary_to_atom(exmpp_xml:get_attribute(Mockup, <<"module">>, <<>>), utf8),
            function=binary_to_atom(exmpp_xml:get_attribute(Mockup, <<"function">>, <<>>), utf8),
            code=bin_to_code(mockup, exmpp_xml:get_path(Mockup, [{element, 'code'}, cdata]))}
    end, Mockups);

parse(#xmlel{name=steps, children=Steps}) ->
    lists:map(fun
        (#xmlel{children=[#xmlel{}=Child]}=Step) ->
            Type = bin_to_type(exmpp_xml:get_attribute(Step, <<"type">>, <<"send">>)),
            #step{
                name=exmpp_xml:get_attribute(Step, <<"name">>, <<"noname">>),
                type=Type,
                times=bin_to_integer(exmpp_xml:get_attribute(Step, <<"times">>, <<"1">>)),
                timeout=bin_to_integer(exmpp_xml:get_attribute(Step, <<"timeout">>, <<"1000">>)),
                stanza=Child};
        (#xmlel{children=[#xmlcdata{}|_]}=Step) ->
            Type = bin_to_type(exmpp_xml:get_attribute(Step, <<"type">>, <<"code">>)),
            #step{
                name=exmpp_xml:get_attribute(Step, <<"name">>, <<"noname">>),
                type=Type,
                times=bin_to_integer(exmpp_xml:get_attribute(Step, <<"times">>, <<"1">>)),
                stanza=bin_to_code(steps, exmpp_xml:get_cdata(Step))}
    end, Steps).

bin_to_code(general, B) ->
    bin_to_code(<<"fun() -> ", B/binary, " end.">>);

bin_to_code(steps, B) ->
    bin_to_code(<<"fun(Packet, PID) -> ", B/binary, " end.">>);

bin_to_code(mockup, B) ->
    GenFun = bin_to_code(<<"fun(PID) -> fun", B/binary, " end end.">>),
    GenFun(self()).

get_defs(Module) ->
    case code:is_loaded(Module) of
        true -> ok;
        _ -> code:load_file(Module)
    end,
    File = code:which(Module),
    Chunks = beam_lib:chunks(File, [abstract_code,"CInf"]),
    {ok,{_Mod,[{abstract_code,{_Version,Forms}},{"CInf",_CB}]}} = Chunks,
    Recs = lists:keysort(2, lists:ukeysort(1, 
        [{N,I,D} || {attribute,I,record,{N,_}}=D <- Forms])),
    [ Def || {_Name,_I,Def} <- Recs ].

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

bin_to_integer(B) ->
    list_to_integer(binary_to_list(B)).

bin_to_type(<<"receive">>) -> 'receive';
bin_to_type(<<"code">>) -> 'code';
bin_to_type(<<"store">>) -> 'store';
bin_to_type(_) -> 'send'.
