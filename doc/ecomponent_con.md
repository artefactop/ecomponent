

# Module ecomponent_con #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#active-1">active/1</a></td><td>Sets the connection active.</td></tr><tr><td valign="top"><a href="#active-2">active/2</a></td><td>Sets the connection active.</td></tr><tr><td valign="top"><a href="#down-1">down/1</a></td><td>Sets the connection as down.</td></tr><tr><td valign="top"><a href="#is_active-1">is_active/1</a></td><td>Checks an ID.</td></tr><tr><td valign="top"><a href="#passive-1">passive/1</a></td><td>Sets the connection passive.</td></tr><tr><td valign="top"><a href="#passive-2">passive/2</a></td><td>Sets the connection passive.</td></tr><tr><td valign="top"><a href="#resolve_id-1">resolve_id/1</a></td><td>Resolve from group ID.</td></tr><tr><td valign="top"><a href="#send-1">send/1</a></td><td>Select a connection and send the stanza.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send the stanza to the specific connection.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>Starts the connection.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stops the connection.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="active-1"></a>

### active/1 ###


<pre><code>
active(ID::atom()) -&gt; {active, atom()}
</code></pre>

<br></br>


Sets the connection active. This function will be used for activate
the connections.
<a name="active-2"></a>

### active/2 ###


<pre><code>
active(ID::atom(), Group::atom()) -&gt; {active, atom()}
</code></pre>

<br></br>


Sets the connection active. This function will be used for activate
the connections. Adds a new parameter named Group for use only this
name instead of the specific (or generated) names in case of pool
is used.
<a name="down-1"></a>

### down/1 ###


<pre><code>
down(ID::atom()) -&gt; {down, atom()}
</code></pre>

<br></br>


Sets the connection as down. This function will be used to report a
down from the worker connection.
<a name="is_active-1"></a>

### is_active/1 ###


<pre><code>
is_active(ID::atom()) -&gt; boolean()
</code></pre>

<br></br>


Checks an ID. If the connection is available as passive or down
returns true, otherwise false.
<a name="passive-1"></a>

### passive/1 ###


<pre><code>
passive(ID::atom()) -&gt; {passive, atom()}
</code></pre>

<br></br>


Sets the connection passive. This function will be used for activate
as passive a connection.
<a name="passive-2"></a>

### passive/2 ###


<pre><code>
passive(ID::atom(), Group::atom()) -&gt; {passive, atom()}
</code></pre>

<br></br>


Sets the connection passive. This function will be used for activate
as passive a connection. Adds a new parameter named Group for use only
this name instead of the specific (or generated) names in case of pool
is used.
<a name="resolve_id-1"></a>

### resolve_id/1 ###


<pre><code>
resolve_id(ID::atom()) -&gt; atom()
</code></pre>

<br></br>


Resolve from group ID. If the ID passed as param is a group, this
function returns an element inside the group in round-robin. Else,
you get the same atom.
<a name="send-1"></a>

### send/1 ###


<pre><code>
send(Info::<strong>exmpp_xml:xmlel()</strong>) -&gt; ok
</code></pre>

<br></br>


Select a connection and send the stanza.
<a name="send-2"></a>

### send/2 ###


<pre><code>
send(Info::<strong>exmpp_xml:xmlel()</strong>, ID::atom()) -&gt; ok
</code></pre>

<br></br>


Send the stanza to the specific connection. If the connection is not
available the stanza will be sent to the first available connection
in the active pool. If the active pool is empty, try to send the
stanza to a passive connection. And finally if this is impossible,
waits a moment (2000ms) and try again.
<a name="start_link-2"></a>

### start_link/2 ###


<pre><code>
start_link(JID::<a href="ecomponent.md#type-jid">ecomponent:jid()</a>, Conf::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; {ok, pid()} | ignore | {error, {already_started, pid()}} | {error, term()}
</code></pre>

<br></br>


Starts the connection. The params needed to start the connection are a JID
in string format and a proplists of configurations with server or node
keys available inside, and others configurations more.
<a name="stop-0"></a>

### stop/0 ###


<pre><code>
stop() -&gt; ok
</code></pre>

<br></br>


Stops the connection.
