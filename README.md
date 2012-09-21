eComponent
==========

Abstract xmpp component written in Erlang/OTP

By default support:
XEP-0030 - Service Discovery
XEP-0199 - XMPP Ping

Configure

app.config

  %% Ecomponent config
	{ecomponent, [
					{jid, "{{jid_var}}" },
					{server, "{{xmpp_server_host}}" },
					{port, {{xmpp_server_port}} },
					{pass, "{{xmpp_server_pass}}" },
					{whitelist, ["check@domain"] }, %% jid of users not affected by throttle
					{max_per_period, {{max_per_period_var}} },
					{period_seconds, {{period_seconds_var}} },
					{processors, [
              {'default', {'app','processor'}} %% indicate your iq_processor
						] }
					]},

vars.config

{jid_var,  	"jid_service"}.
{xmpp_server_host,	"host"}. %% server host
{xmpp_domain_var,  "domain"}. %% server domain
{xmpp_server_port,	8888}. %% connection port
{xmpp_server_pass,	"pass"}. %% connection pass
{max_per_period_var,	10}.
{period_seconds_var,	6}.

On processors you can indicate a list of processor by namespace or choose one by default,
the processor can be a module or an app.
