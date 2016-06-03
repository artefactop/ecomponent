

# Module ecomponent_con_worker #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>Starts an individual connection.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stops an individual connection.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start_link-3"></a>

### start_link/3 ###


<pre><code>
start_link(ID::{atom(), atom()}, JID::<a href="ecomponent.md#type-jid">ecomponent:jid()</a>, Conf::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; {ok, pid()} | ignore | {error, {already_started, pid()}} | {error, term()}
</code></pre>

<br></br>


Starts an individual connection. The connection can be to a server or
another node in the cluster.
<a name="stop-1"></a>

### stop/1 ###


<pre><code>
stop(ID::atom()) -&gt; ok
</code></pre>

<br></br>


Stops an individual connection.
