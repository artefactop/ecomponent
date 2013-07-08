-include_lib("exmpp/include/exmpp_jid.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("confetti/include/confetti.hrl").
-include("../include/ecomponent.hrl").

-type jid() :: { Name::string(), Server::string(), Resource::string() }.

-type mod_processor() :: {mod, Module::atom()}.

-type app_processor() :: {app, App::atom()}.

-type message_processor() :: mod_processor() | app_processor().

-type presence_processor() :: mod_processor() | app_processor().

-type processor() :: {Name::atom(), Value::(mod_processor() | app_processor())}.

-type accesslist() :: Domains::list(binary()).

-export_type([jid/0]).

-define(MAX_PER_PERIOD, 10).
-define(PERIOD_SECONDS, 6).
-define(MAX_TRIES, 3).
-define(RESEND_PERIOD, 100).
-define(REQUEST_TIMEOUT, 10).
-define(SYSLOG_FACILITY, local7).
-define(SYSLOG_NAME, "ecomponent").

-record(state, {
    jid :: jid(),
    whiteList = [] :: list(string()),
    maxPerPeriod = ?MAX_PER_PERIOD :: integer(),
    periodSeconds = ?PERIOD_SECONDS :: integer(),
    processors :: list(processor()),
    message_processor :: message_processor(),
    presence_processor :: presence_processor(),
    maxTries = ?MAX_TRIES :: integer(),
    resendPeriod = ?RESEND_PERIOD :: integer(),
    requestTimeout = ?REQUEST_TIMEOUT :: integer(),
    accessListSet = [] :: accesslist(),
    accessListGet = [] :: accesslist(),
    syslogFacility = ?SYSLOG_FACILITY :: atom(),
    syslogName = ?SYSLOG_NAME :: string(),
    timeout = undefined :: integer(),
    features = [] :: [binary()],
    info = [] :: proplists:proplists(),
    disco_info = true :: boolean()
}).
