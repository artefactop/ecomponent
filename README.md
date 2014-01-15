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

###ecomponent

- `syslog_name::string()` default value is `ecomponent`. Optional.  
- `jid::string()` external component domain.  
- `server::string()` xmpp server host.  
- `port::integer()` xmpp server port.  
- `pass::string()` xmpp server password.  
- `whitlist::list(domain::Binary)` a list of domain for their request are not dropped, other way the request will be dropped if send more than 10 request in minus 6 seconds by default. Can be empty.  
- `access_list_get::list(tuple(Atom, list(Binary)))` a list of {Namespace, list(domain::Binary)}. You can configure what domain can send iq get by namespace. Can be empty.  
- `access_list_set::list(tuple(Atom, list(Binary)))` a list of {Namespace, list(domain::Binary)}. You can configure what domain can send iq set by namespace. Can be empty.
- `max_per_period::integer()` number of reques per period for drop packets. Optional.  
- `period_seconds::integer()` amount of time for period. Optional.  
- `processors` configure what module or app will be process the iq's. You can set a default value for all namespaces or configure by pair `{namespace::Atom, mod_processor | app_processor}` where `mod_processor:: {mod, Module::atom()}` and  `app_processor:: {app, App::atom()}`.  
- `message_processor` set one processor for messages. Optional.  
- `presence_processor` set one processor for presence stanzas. Optional.  
You can set the same processor for iq's, messages and presences or not.  
- `mnesia_nodes` set name of mnesia node if you want set your external component like cluster. Optional.  
- `mnesia_callback` identify a function that should returns a list of tables to create and distribute in the mnesia cluster. Optional.
- `info` part of disco#info identity, as you can see in [XEP-0030 - Service Discovery](http://xmpp.org/extensions/xep-0030.html). You can setup the type of identity and the name. Optional.
- `disco_info` setup if the disco#info should be showed (true) or muted (false). Default value is `true`. Optional.

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

###clustering

If you use Mnesia to do a share info in a serveral nodes cluster, you can create your tables as follow:

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
...
    {mnesia_callback, [
        {mycode, create_tables, []}
    ]}
...
```
