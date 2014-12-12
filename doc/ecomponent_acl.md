

# Module ecomponent_acl #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#access_list_get-2">access_list_get/2</a></td><td>check if is allowed for this JID send an IQ GET with this NameSpace.</td></tr><tr><td valign="top"><a href="#access_list_set-2">access_list_set/2</a></td><td>check if is allowed for this JID send an IQ SET with this NameSpace.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Starts the ecomponent main server.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stops the ecomponent main server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="access_list_get-2"></a>

### access_list_get/2 ###


<pre><code>
access_list_get(NS::atom(), Jid::<a href="#type-jid">jid()</a>) -&gt; boolean()
</code></pre>

<br></br>


check if is allowed for this JID send an IQ GET with this NameSpace.
<a name="access_list_set-2"></a>

### access_list_set/2 ###


<pre><code>
access_list_set(NS::atom(), Jid::<a href="#type-jid">jid()</a>) -&gt; boolean()
</code></pre>

<br></br>


check if is allowed for this JID send an IQ SET with this NameSpace.
<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, Pid::pid()} | {error, Reason::any()}
</code></pre>

<br></br>


Starts the ecomponent main server.
<a name="stop-0"></a>

### stop/0 ###


<pre><code>
stop() -&gt; ok
</code></pre>

<br></br>


Stops the ecomponent main server.
