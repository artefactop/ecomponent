eComponent
==========

[![Build Status](https://api.travis-ci.org/altenwald/ecomponent.png)](https://travis-ci.org/altenwald/ecomponent)

Framework for write XMPP external components [XEP-0114](http://xmpp.org/extensions/xep-0114.html) in Erlang/OTP

By default support:

 * [XEP-0030 - Service Discovery](http://xmpp.org/extensions/xep-0030.html)
 * [XEP-0199 - XMPP Ping](http://xmpp.org/extensions/xep-0199.html)

- - -

Configuration
-------------

###ecomponent (general params)

The basic configuration for the ecomponent project is as follow:

- `syslog_name::string()` default value is `ecomponent`. Optional.  
- `jid::string()` external component domain.  

###processors

The processors handle requests depend on if the stanza is an IQ, a presence or a messsage:

- `processors` configure what module or app will be process the iq's. You can set a `default` value for all namespaces or configure by pair `{namespace::atom() | default, mod_processor | app_processor}` where `mod_processor:: {mod, Module::atom()}` and  `app_processor:: {app, App::atom()}`.  
- `message_processor` set one processor for messages. Optional.  
- `presence_processor` set one processor for presence stanzas. Optional.  

A processor could be an application `{app, App::atom()}` or a module `{mod, Module::atom()}`. By example, if you want to handle only messages and pass the processing task to a module, you can set this in the config file:

```erlang
{message_processor, {mod, my_message_processor}}
```

In this example the module `my_message_processor` must exists and must have the following function:

```erlang
-module(my_message_processor).
-export([process_message/1]).

-include_lib("ecomponent/include/ecomponent.hrl").

process_message(#message{}=Message) ->
    ok.
```

The record `#message{}` can be found [here](https://github.com/altenwald/ecomponent/blob/master/include/ecomponent.hrl#L41).

Now, if we want to add processors for IQ and presence, the configuration should be:

```erlang
{presence_processor, {mod, my_processor}},
{message_processor, {mod, my_processor}},
{processors, [
    {default, {mod, my_processor}}
]}
```

Note that you can change `default` for one namespace and filter the requests by namespace in diferent modules or applications.

The code to use this configuration should be:

```erlang
-module(my_processor).
-export([
    process_presence/1,
    process_message/1,
    process_iq/1
]).

-include_lib("ecomponent/include/ecomponent.hrl").

process_presence(#presence{}=Presence) ->
    ok.

process_message(#message{}=Message) ->
    ok.

process_iq(#params{}=Params) ->
    ok.
```

The use of `app` instead of `mod` is only recommended when a state should be kept between requests. Be careful with this, because the use of `app` could generate a bottleneck.

###server configuration

You can configure only one server to connect through XMPP, several connections (each one with its own id) or shared connections (connections through another node in the cluster).

If you have three servers and you want to connect each one locally, but share the connection to avoid when a connection is lost in one server, you can configure as follow:

```erlang
%% ecomponent one:
{servers, [
    {ejabberd_one, [
        {server, "localhost"},
        {port, 5252},
        {secret, "mypass"}
    ]},
    {ejabberd_two, [
        {node, 'ecomponent_two@server_two'}
    ]},
    {ejabberd_three, [
        {node, 'ecomponent_three@server_three'}
    ]}
]}

%% ecomponent two:
{servers, [
    {ejabberd_one, [
        {node, 'ecomponent_one@server_one'}
    ]},
    {ejabberd_two, [
        {server, "localhost"},
        {port, 5252},
        {secret, "mypass"}
    ]},
    {ejabberd_three, [
        {node, 'ecomponent_three@server_three'}
    ]}
]}

%% ecomponent three:
{servers, [
    {ejabberd_one, [
        {node, 'ecomponent_one@server_one'}
    ]},
    {ejabberd_two, [
        {node, 'ecomponent_two@server_two'}
    ]},
    {ejabberd_three, [
        {server, "localhost"},
        {port, 5252},
        {secret, "mypass"}
    ]}
]}
```

When the server wants to send a message always try to send to the local connection but, if this is down, the message will be tried by the connections in another nodes in the cluster.

###throttle configuration

If you want to avoid a collapse for your component, you can configure a throttle with the follow params inside the `ecomponent` main tag:

- `throttle::boolean()` enable/disable the throttle. `true` by default. Optional.
- `whitelist::list(domain::Binary)` a list of domain for their request are not dropped, other way the request will be dropped if send more than 10 request in minus 6 seconds by default. Can be empty.  
- `max_per_period::integer()` number of reques per period for drop packets. Optional.  
- `period_seconds::integer()` amount of time for period. Optional.  

All the request arriving to the component will be anoted in `mod_monitor`. If the requests for one user achieve more than `max_per_period` in the `period_seconds`, the requests from this JID will be dropped until the conditions will be false again.

All the JIDs inside `whitelist` will be ignored by the monitor.

###granting access to IQs

The requests to IQs can be filtered by users (JIDs) using ACLs. This ACLs are set by namespace, type and JID. A user (JID) can be granted for a namespace in a specific type. The params for configuration inside `ecomponent` tag are the following:

- `access_list_get::list(tuple(Atom, list(Binary)))` a list of {Namespace, list(domain::Binary)}. You can configure what domain can send iq get by namespace. Can be empty.  
- `access_list_set::list(tuple(Atom, list(Binary)))` a list of {Namespace, list(domain::Binary)}. You can configure what domain can send iq set by namespace. Can be empty.

It's possible specify that `user1@domain.com` can access to `jabber:iq:time` for the component, but only in `get` type:

```erlang
{access_list_get, [
    <<"user1@domain.com">>
]}
```

Note that if the configuration for these parameters is omitted all the users can access to all the IQs.

###disco info

There are a couple of params to configure the behaviour when the server requires a `disco#info` from the component. In this case we can use set a reply like this:

```xml
  <query xmlns='http://jabber.org/protocol/disco#info'>
    <identity
        category='conference'
        type='text'
        name='Play-Specific Chatrooms'/>
    <feature var='http://jabber.org/protocol/disco#info'/>
    <feature var='http://jabber.org/protocol/disco#items'/>
    <feature var='http://jabber.org/protocol/muc'/>
    <feature var='jabber:iq:register'/>
    <feature var='jabber:iq:search'/>
    <feature var='jabber:iq:time'/>
    <feature var='jabber:iq:version'/>
  </query>
```

Only we need to add this configuration to the config file:

```erlang
{features, [
    <<"http://jabber.org/protocol/disco#info">>,
    <<"http://jabber.org/protocol/disco#items">>,
    <<"http://jabber.org/protocol/muc">>,
    <<"jabber:iq:register">>,
    <<"jabber:iq:search">>,
    <<"jabber:iq:time">>,
    <<"jabber:iq:version">>
]},
{info, [
    {type, <<"text">>},
    {name, <<"Play-Specific Chatrooms">>},
    {category, <<"conference">>}
]},
{disco_info, true}
```

###clustering with mnesia

Scale ecomponent is pretty easy with these params:

- `mnesia_nodes::[node()]` set name of mnesia node if you want set your external component like cluster. Optional.  
- `mnesia_callback` identify a function that should returns a list of tables to create and distribute in the mnesia cluster. Optional.

With `mnesia_nodes` it's possible to communicate the nodes in a easy way, and auto-configure Mnesia to work in cluster.

As a plus, it's possible use Mnesia for create new tables easily inside the cluster. By example:

```erlang
-module(mycode).
-compile([export_all]).

-record(mytable, {id, name, description}).

create_tables() -> [
    {mytable, disc_copies, record_info(fields, mytable)}
].
```

And in the config file:

```erlang
{mnesia_callback, [
    {mycode, create_tables, []}
]}
```

###folsom

Dependency: [folsom](https://github.com/boundary/folsom_cowboy)

Automatically ecomponent provide you metrics using folsom. In old versions we use [folsom_cowboy](https://github.com/altenwald/folsom_cowboy) but it's better you can select the better way for you to collect this data.

The default values are:

- iq throughput by namespace. [spiral](https://github.com/boundary/folsom#spiral-meter).
- iq response time by namespace. [slide](https://github.com/boundary/folsom#slide) 60 sec.
- iq dropped by namespace. [spiral](https://github.com/boundary/folsom#spiral-meter).
- presence throughput by type (available, unavailable, ...). [spiral](https://github.com/boundary/folsom#spiral-meter).
- presence dropped by type. [spiral](https://github.com/boundary/folsom#spiral-meter).
- message throughput by type (normal, chat, groupchat, ...). [spiral](https://github.com/boundary/folsom#spiral-meter).
- message dropped by type. [spiral](https://github.com/boundary/folsom#spiral-meter).

###lager

Dependency: [lager configuration](https://github.com/basho/lager#configuration)

###Config file example

The example file `app.config` have the following sections:
```
    [
        {ecomponent, [
            {syslog_name, ComponentName :: string()},
            {jid, ComponentDomain :: string()},

            {servers, [
                {server_one, [
                    {server, ServerHost :: string()},
                    {port, ServerPort :: non_neg_integer()},
                    {pass, ServerPass :: string()}
                ]},
                {server_shared, [
                    {node, Node :: atom()}
                ]}
            ]},

            {throttle, boolean()},
            {whitelist, [domain::Binary] }, %% throttle whitelist
            {max_per_period, MaxPerPeriod :: non_neg_integer()},
            {period_seconds, PeriodSeconds :: non_neg_integer()},

            {access_list_get, [{namespace::Atom, [domain::Binary]}]},
            {access_list_set, [{namespace::Atom, [domain::Binary]}]},

            {processors, [
                {default, mod_processor | app_processor}
            ]},
            {message_processor, message_processor},
            {presence_processor, presence_processor},

            {mnesia_nodes, [Node::atom()]},
            {mnesia_callback, [Callback::{M::atom(),F::atom(),A::[term()]}]},

            {features, [
                <<"jabber:iq:last">> | [binary()]
            ]},
            {info, [
                {type, <<"jabber:protocol:boot">>},
                {name, <<"Boot">>},
                {category, <<"text">>}
            ]},
            {disco_info, boolean()}
        ]},
    
        {lager, [
            {handlers, [
                {lager_console_backend, info},
                {lager_file_backend, [
                    {"log/error.log", error, 10485760, "$D0", 5},
                    {"log/info.log", info, 10485760, "$D0", 5}
                ]}
            ]},
            {crash_log, "log/crash.log"},
            {crash_log_msg_size, 65536},
            {crash_log_size, 10485760},
            {crash_log_date, "$D0"},
            {crash_log_count, 5},
            {error_logger_redirect, true}
        ]}
    ].
```

On processors you can indicate a list of processor by namespace or choose one by default,
the processor can be a module or an app.
