eComponent
==========

[![Build Status](https://api.travis-ci.org/bosqueviejo/ecomponent.png)](https://travis-ci.org/manuel-rubio/ecomponent)

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

###[folsom_cowboy](https://github.com/bosqueviejo/folsom_cowboy)

Automatically ecomponent provide you metrics, you can set up the port when you want read it, `port::integer()`.  

The default values are:

- iq throughput by namespace. [spiral](https://github.com/boundary/folsom#spiral-meter).
- iq response time by namespace. [slide](https://github.com/boundary/folsom#slide) 60 sec.
- iq dropped by namespace. [spiral](https://github.com/boundary/folsom#spiral-meter).

###confetti
[confetti configuration](https://github.com/manuel-rubio/confetti)

###lager
[lager configuration](https://github.com/basho/lager#configuration)

The example file `app.config` have the following sections:
```
    [
        {ecomponent, [
            {syslog_name, "{{component_name}}" },
            {jid, "{{component_name}}.{{xmpp_domain}}" },
            {server, "{{xmpp_server_host}}" },
            {port, {{xmpp_server_port}} },
            {pass, "{{xmpp_server_pass}}" },
            {whitelist, [domain::Binary] }, %% throttle whitelist
            {access_list_get, [{namespace::Atom, [domain::Binary]}]},
            {access_list_set, [{namespace::Atom, [domain::Binary]}]},
            {max_per_period, {{max_per_period}} },
            {period_seconds, {{period_seconds}} },
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
            {disco_info, boolean()}
        ]},
    
        {folsom_cowboy,[
            {port, 5565}
        ]},

        {confetti, [
            {mgmt_config_location, {"app.config", "etc"}},
            %{plugins, [myapp_conf::Atom]},
            {port, 50000}
        ]},
    
        {lager, [
            {handlers, [
                {lager_console_backend, info},
                {lager_file_backend, [
                    {"{{platform_log_dir}}/error.log", error, 10485760, "$D0", 5},
                    {"{{platform_log_dir}}/info.log", info, 10485760, "$D0", 5}
                ]}
            ]},
            {crash_log, "{{platform_log_dir}}/crash.log"},
            {crash_log_msg_size, 65536},
            {crash_log_size, 10485760},
            {crash_log_date, "$D0"},
            {crash_log_count, 5},
            {error_logger_redirect, true}
        ]}
    ].
```

The file `vars.config` completes the app.config with simple format to configure specific parameters:

```
    %% Platform-specific installation paths
    {platform_bin_dir,   "./bin"}.
    {platform_data_dir,  "./data"}.
    {platform_etc_dir,   "./etc"}.
    {platform_lib_dir,   "./lib"}.
    {platform_log_dir,   "./log"}.
    
    %% ecomponent
    {xmpp_server_host,  "localhost"}.
    {xmpp_server_port,  8989}.
    {xmpp_server_pass,  "secret"}.
    {max_per_period,    10}.
    {period_seconds,    6}.
    {xmpp_domain,       "localhost"}.
    {component_name,    "mycomponent"}. %% subdomain of jid
```

On processors you can indicate a list of processor by namespace or choose one by default,
the processor can be a module or an app.
