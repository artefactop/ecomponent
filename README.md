eComponent
==========

Abstract xmpp component written in Erlang/OTP

By default support:

 * [XEP-0030 - Service Discovery](http://xmpp.org/extensions/xep-0030.html)
 * [XEP-0199 - XMPP Ping](http://xmpp.org/extensions/xep-0199.html)

Configure
---------

The file `app.config` must have the following sections:

    [
        {ecomponent, [
            {syslog_name, "{{component_name}}" },
            {jid, "{{component_name}}.{{xmpp_domain}}" },
            {server, "{{xmpp_server_host}}" },
            {port, {{xmpp_server_port}} },
            {pass, "{{xmpp_server_pass}}" },
            {whitelist, [] }, %% throttle whitelist
            {access_list_get, []},
            {access_list_set, []},
            {max_per_period, {{max_per_period}} },
            {period_seconds, {{period_seconds}} },
            {processors, [
                {default, {mod:: atom(), processor:: atom()}} %% | {app:: atom(), processor:: atom()}
                %% {namespace :: atom(), {app :: atom(), app_processor:: atom()}}
            ]},
            {message_processor, {mod:: atom(), mod_app_processor:: atom()}} %% | {app:: atom(), app_message_processor:: atom()}
        ]},
    
        {folsom_cowboy,[
            {port, 5565}
        ]},

        {confetti, [
            {mgmt_config_location, {"app.config", "etc"}},
            %{plugins, [myapp_conf]},
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

The file `vars.config` completes the app.config with simple format to configure specific parameters:

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

On processors you can indicate a list of processor by namespace or choose one by default,
the processor can be a module or an app.

