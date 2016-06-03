# Testing

For test your component you can use the functional test system that is included inside ecomponent. This system has been defined in use of several projects and is still under development so, if you want to do a contribution, you can open an issue or send us a pull request.

## Creating the test suite

You need to create a `test` and `test/functional` directories. The `test` directory should include test files like in `eunit` system. You can do an easy implementation creating files with this content:

```erlang
-module(my_test).
-compile([export_all]).

disco_info_test_() ->
	ecomponent_func_test:check([
		"disco_info_test",
		"disco_items_test",
		"ping_test"
	]).
```

This file will be executed by `eunit` and calls to `ecomponent_func_test` to do the `check` of you functional tests. Your functional tests should be in the `functional` directory and their names will be (as described above): `disco_info_test.xml`, `disco_items_test.xml` and `ping_test.xml`.

The `check` function can be called as `check(Tests)`, `check(Tests,Timeout)` or `check(Tests,Timeout,Verbose)`. The `Timeout` variable is for indicate a timeout value for the `eunit` testing system. `Verbose` can be set as `true` or `false` for do the mocks for `lager` and `syslog` as verbose or not.

## Create a Functional Test

The XML format for run the functional test is as follow:

```xml
<functional>
    <config>
        <syslog name="ecomponent"/>
        <jid>ecomponent.test</jid>
        <throttle active="false"/>
        <processors>
            <iq xmlns="default" type="mod" data="dummy"/>
            <message type="mod" data="dummy"/>
            <presence type="mod" data="dummy"/>
        </processors>
        <disco-info active="true">
            <feature var="jabber:iq:last"/>
            <identity type="jabber:last" name="Last Component" category="text"/>
        </disco-info>
    </config>
    <steps>
        <step name="send disco#info" type="send">
            <iq xmlns='jabber:client'
                type='get'
                to='alice.localhost'
                from='bob@localhost/pc'
                id='test_bot'>
                <query xmlns='http://jabber.org/protocol/disco#info'/>
            </iq>
        </step>
        <step name="receive creation messages" type="receive">
            <iq xmlns='jabber:client'
                type='result'
                id='test_bot'
                from='alice.localhost'
                to='bob@localhost/pc'>
                <query xmlns='http://jabber.org/protocol/disco#info'>
                    <identity type='jabber:last' 
                              name='Last Component' 
                              category='text'/>
                    <feature var='jabber:iq:last'/>
                </query>
            </iq>
        </step>
    </steps>
</functional>
```

### The config section

The first you need to do is configure the ecomponent for launch the test. The configuration will be important to achieve the system can send you the information. The configuration is similar as you can see in the configuration section about the `app.config` file.

* `syslog`: this configuration is silent by the moment.
* `jid`: the JID for the component.
* `throttle`: the throttle section should be configured as `off` or `on` with params. You can see several examples:

```xml
<!-- throttle off -->
<throttle active="false"/>
```

```xml
<!-- throttle on -->
<throttle active="true" max-per-period="10" period-seconds="5"/>
```

```xml
<!-- throttle on, with whitelist -->
<throttle active="true" max-per-period="10" period-seconds="5">
	<whitelist>
		<item jid="localhost"/>
		<item jid="conference.localhost"/>
	</whitelist>
</throttle>
```
* `processors`: where the stanzas should go. We can configure as:

```xml
<!-- only messages to 'muc' module -->
<processors>
	<message type="mod" data="muc"/>
</processors>
```

```xml
<!-- only presences to 'users' app -->
<processors>
	<presence type="app" data="users"/>
</processors>
```

```xml
<!-- several namespaces for iq stanzas to several modules -->
<processors>
	<iq xmlns="jabber:iq:last" type="mod" data="last"/>
	<iq xmlns="http://jabber.org/protocol/ping" type="app" data="ping"/>
</processors>
```

```xml
<!-- a full example -->
<processors>
	<iq xmlns="default" type="mod" data="dummy_iq"/>
	<message type="mod" data="dummy_message"/>
	<presence type="mod" data="dummy_presence"/>
</processors>
```

* `disco-info`: is present as will sent to clients, but the main tag can be setted with its attribute `active` as `true` or `false`. If you set the `active` attribute as `false` you can leave empty the `disco-info` tag.

* `access-list-get` and `access-list-set`: is used when you want block incoming stanzas from client (set or get) with a specific jid. You must specify the iq NS and its children are a list of items blocked, example:

```xml
<!-- a full example -->
<access-list-get>
    <iq xmlns="urn:itself">
        <item value="alice@localhost"/>
    </iq>
</access-list-get>
<access-list-set>
    <iq xmlns="urn:itself">
        <item value="alice.localhost"/>
    </iq>
</access-list-set>
```

### Mock-ups

The code to be tested can be using a database or an external connection or something that could be not available in the test environment. The mock-up help simulating the calls to this elements and give always the same response.

For example, if you use [`emysql`](https://github.com/Eonblast/Emysql) for connecto to MySQL and you want to do a mock-up, you perhaps wants to write a mockup for fake the calls to `add_pool` and `execute` functions:

```xml
<mockups>
	<mockup module="emysql" function="add_pool">
		<code><![CDATA[
			(_,_,_,_,_,_,_,_) -> ok
		]]></code>
	</mockup>
	<mockup module="emysql" function="execute">
		<code><![CDATA[
			(_,<<"SELECT * FROM users WHERE id = ",_/binary>>) ->
				{result_packet, 1, [<<"id">>,<<"name">>], [], []}
		]]></code>
	</mockup>
    <mockup module="emysql" function="query">
        <code module="emysql_fake" function="query" arity="3"/>
    </mockup>
    <mockup module="emysql" function="equery">
        <code module="emysql_fake" function="equery" arity="3" send-pid="yes"/>
    </mockup>
</mockups>
```

This system uses internally [`meck`](https://github.com/eproxus/meck) the call for this is equivalent to `meck:expect/3`.

The arity for the code defined should be the same that the function you want to replace (or mock-up).

The `mockups` tag have the following attributes you can use to improve the use of the mocks:

- *passthrough*: when you want to mock a module but not all the functions the module have, you can use this attribute setting as `true`.
- *strict*: set this attribute to `true` if you need to mock a module that doesn't exists.

You can define the code inline (inside the XML doc) or outside adding to the code
tag the attributes `module`, `function` and `arity`.

If you use `send-pid="yes"` the PID will be sent as first param increasing the number of params accepted by the function. In the last example above, the function called should be as follow:

```erlang
equery(PID, _Pool, _Query, _Args) ->
    PID ! select,
    [].
```

### Start and Stop Codes

When you start the test you can execute a code to launch your system, or part of them, the part you want to test. The code should be prepared to be started by code and stopped in the same way. An example:

```xml
<start-code><![CDATA[
	muc_app:start([], [])
]]></start-code>

<stop-code><![CDATA[
	muc_app:stop([])
]]></stop-code>
```

The code could be used from an external module using the params `module` and `function` (the arity is supposed to be zero) for the `start-code` and/or `stop-code` tags:

```xml
<start-code module="muc_app" function="start"/>
<stop-code module="muc_app" function="stop"/>
```

The Erlang code for those functions should be something like:

```erlang
-module(muc_app).
-export([start/0, stop/0]).

start() ->
    % actions to start
    ok.

stop() ->
    % actions to stop
    ok.
```

### Step by step

The steps should be executed in the order appears in the file. The log show the `name` of the test and depends on the type, should perform some of the next actions:

#### send

Send to `ecomponent` the stanza describe inside the `step` tag. As is. The stanza should be stored too in the `Packet` variable for use it in the next `code` stanza.

```xml
<step name="send disco#info" type="send">
    <iq xmlns='jabber:client'
        type='get'
        to='alice.localhost'
        from='bob@localhost/pc'
        id='test_bot'>
        <query xmlns='http://jabber.org/protocol/disco#info'/>
    </iq>
</step>
```

#### receive

Waits for receive the stanza describe inside the `step` tag from `ecomponent`. In this case you can use the wildcard `{{_}}`. This wildcard indicates the value for this attribute or CDATA should be whatever.

```xml
<step name="receive creation messages" type="receive">
    <iq xmlns='jabber:client'
        type='result'
        id='test_bot2'
        from='ecomponent.test'/>
</step>
```

You can add more than one stanza inside the step, all the stanzas should arrive (in whatever order) so, it's very useful if you want to receive more than one stanza and you don't know the order you'll receive them:

```xml
<step name="receive creation messages" type="receive">
    <iq xmlns='jabber:client'
        type='result'
        id='test_bot2'
        from='ecomponent.test'/>
    <message type='chat' id='test_bot3' from='ecomponent.test'>
    	<body>created!</body>
    </message>
</step>
```

Even you can configure `receive` for intercept process messages as code:

```xml
<step name="receive creation messages (db)" type="receive"><![CDATA[
    updated
]]></step>
```

This is equivalent to have in that part the following code:

```erlang
receive
    updated -> ok;
    Other -> throw(Other)
after Timeout ->
    throw("TIMEOUT!!!")
end
```

#### store

Save the stanza inside the `step` tag in the variable `Packet` for use in the next step, that should be `code`. The stanza should be parsed before store it.

```xml
<step name="request arrived" type="store">
    <iq xmlns='jabber:client'
        type='set'
        from='bob@localhost/res'
        to='alice.localhost'
        id='test_bot_save_id_generated_test'>
        <data xmlns='urn:itself'/>
    </iq>
</step>
```

#### code

Runs a code inside the `step` tag.

```xml
<step name="update save_id" type="code"><![CDATA[
    Id = <<"test_bot_save_id_generated_test">>,
    ecomponent:save_id(Id, 'urn:itself', Packet, dummy),
    ecomponent ! getup
]]></step>
```

or:

```xml
<step name="update save_id" type="code">
    <code module="myapp_test" function="save_id"/>
</step>
```

The function `save_id/2` should exists in the `myapp_test` module. The params passed to the function are the previous packet (last XML stanza if there was or `undefined` instead) and the PID.

#### quiet

Waits a specific time and if something is received in that time, throw an error.

```xml
<step name="nothing should be received" type="quiet" timeout="1000"/>
```

The params for the `step` tag are the following:

* `name`: the information that you can see when the test is running.
* `type`: the type of step to be executed.
* `times`: the times the test should be executed. Is very useful for `receive` several stanzas with different data inside but the same structure.
* `timeout`: the time that the `receive` code should be await until throw the timeout error.

## Running the tests

For launch the tests you can use the `eunit` system:

```
./rebar eunit skip_deps=true
```

The output for `ecomponent` tests are similar to this:

```
module 'ecomponent_test'
   ** TEST => processor_iq_test
    ecomponent_func_test: run (start)...[0.002 s] ok
    ecomponent_func_test: run_steps (send: an unknown namespace)...[0.006 s] ok
    ecomponent_func_test: run_steps (code: checks)...[0.018 s] ok
    ecomponent_func_test: run_steps (receive: feature-not-implemented error)...[0.031 s] ok
    ecomponent_func_test: run (stop)...[0.001 s] ok
    [done in 0.073 s]
   ** TEST => processor_message_test
    ecomponent_func_test: run (start)...[0.002 s] ok
    ecomponent_func_test: run_steps (send: a message without processor)...[0.008 s] ok
    ecomponent_func_test: run_steps (quiet: nothing)...[1.002 s] ok
    ecomponent_func_test: run (stop)...[0.001 s] ok
    [done in 1.026 s]
   ** TEST => processor_presence_test
    ecomponent_func_test: run (start)...[0.001 s] ok
    ecomponent_func_test: run_steps (send: an unknown namespace)...[0.004 s] ok
    ecomponent_func_test: run_steps (quiet: nothing)...[1.001 s] ok
    ecomponent_func_test: run (stop)...[0.001 s] ok
    [done in 1.021 s]
   ** TEST => save_id_expired_test
    ecomponent_func_test: run (start)...[0.001 s] ok
    ecomponent_func_test: run_steps (store: request arrived)...ok
    ecomponent_func_test: run_steps (code: update save_id)...[0.005 s] ok
    ecomponent_func_test: run_steps (receive: data urn:itself)...[1.998 s] ok
    ecomponent_func_test: run (stop)...[0.002 s] ok
    [done in 2.022 s]
   ** TEST => disco_muted_test
    ecomponent_func_test: run (start)...[0.002 s] ok
    ecomponent_func_test: run_steps (send: request disco#info)...[0.005 s] ok
    ecomponent_func_test: run_steps (send: ping)...[0.005 s] ok
    ecomponent_func_test: run_steps (receive: ping result)...ok
    ecomponent_func_test: run (stop)...[0.001 s] ok
    [done in 0.028 s]
   ** TEST => ping_test
    ecomponent_func_test: run (start)...[0.002 s] ok
    ecomponent_func_test: run_steps (send: ping)...[0.005 s] ok
    ecomponent_func_test: run_steps (receive: ping result)...ok
    ecomponent_func_test: run (stop)...[0.001 s] ok
    [done in 0.020 s]
   ** TEST => disco_test
    ecomponent_func_test: run (start)...[0.001 s] ok
    ecomponent_func_test: run_steps (send: disco#info)...[0.006 s] ok
    ecomponent_func_test: run_steps (receive: disco#info)...ok
    ecomponent_func_test: run (stop)...[0.001 s] ok
    [done in 0.020 s]
   ** TEST => disco_info_test
    ecomponent_func_test: run (start)...[0.002 s] ok
    ecomponent_func_test: run_steps (send: disco#info)...[0.005 s] ok
    ecomponent_func_test: run_steps (receive: creation messages)...ok
    ecomponent_func_test: run (stop)...[0.001 s] ok
    [done in 0.020 s]
   ** TEST => forward_response_module_test
    ecomponent_func_test: run (start)...[0.002 s] ok
    ecomponent_func_test: run_steps (send: an error)...[0.009 s] ok
    ecomponent_func_test: run_steps (code receive: a response)...ok
    ecomponent_func_test: run (stop)...[0.001 s] ok
    [done in 0.024 s]
   ** TEST => forward_ns_in_set_test
    ecomponent_func_test: run (start)...[0.061 s] ok
    ecomponent_func_test: run_steps (send: to dummy process)...[0.005 s] ok
    ecomponent_func_test: run_steps (receive: the stanza by dummy process)...ok
    ecomponent_func_test: run (stop)...[0.003 s] ok
    [done in 0.083 s]
   ** TEST => message_test
    ecomponent_func_test: run (start)...[0.066 s] ok
    ecomponent_func_test: run_steps (send: message to ecomponent)...[0.006 s] ok
    ecomponent_func_test: run_steps (code receive: message)...ok
    ecomponent_func_test: run (stop)...[0.003 s] ok
    [done in 0.087 s]
   ** TEST => presence_test
    ecomponent_func_test: run (start)...[0.058 s] ok
    ecomponent_func_test: run_steps (send: presence to ecomponent)...[0.005 s] ok
    ecomponent_func_test: run_steps (code receive: presence)...ok
    ecomponent_func_test: run (stop)...[0.003 s] ok
    [done in 0.078 s]
   ** TEST => sync_send_test
    ecomponent_func_test: run (start)...[0.001 s] ok
    ecomponent_func_test: run_steps (store: save Packet)...ok
    ecomponent_func_test: run_steps (code: request Packet as sync_send())...[0.005 s] ok
    ecomponent_func_test: run_steps (receive: the request from sync_send())...ok
    ecomponent_func_test: run_steps (send: the result packet to sync_send())...[0.007 s] ok
    ecomponent_func_test: run_steps (code receive: check params)...ok
    ecomponent_func_test: run (stop)...[0.001 s] ok
    [done in 0.036 s]
   ** TEST => multiconnection_test
    ecomponent_func_test: run (start)...[0.045 s] ok
    ecomponent_func_test: run_steps (send: send to server_to)...[0.005 s] ok
    ecomponent_func_test: run_steps (code receive: timem gives information)...ok
    ecomponent_func_test: run_steps (receive: receive from server_two)...ok
    ecomponent_func_test: run (stop)...[0.038 s] ok
    [done in 0.103 s]
   ** TEST => multiping_test
    ecomponent_func_test: run (start)...[0.001 s] ok
    ecomponent_func_test: run_steps (send: ping)...[0.007 s] ok
    ecomponent_func_test: run_steps (send: ping)...[0.006 s] ok
    ecomponent_func_test: run_steps (send: ping)...[0.005 s] ok
    ecomponent_func_test: run_steps (receive: multi-stanza ping results)...ok
    ecomponent_func_test: run (stop)...[0.001 s] ok
    [done in 0.038 s]
   ** TEST => forward_acl_ns_in_set_test
    ecomponent_func_test: run (start)...[0.067 s] ok
    ecomponent_func_test: run_steps (send: to dummy process)...[0.006 s] ok
    ecomponent_func_test: run_steps (receive: forbidden error)...ok
    ecomponent_func_test: run (stop)...[0.004 s] ok
    [done in 0.089 s]
```

Enjoy!
