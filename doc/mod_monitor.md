

# Module mod_monitor #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accept-3">accept/3</a></td><td>Check if the packet can be accepted.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>Init the monitor.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accept-3"></a>

### accept/3 ###


<pre><code>
accept(Id::string(), Max::integer(), Period::integer()) -&gt; boolean()
</code></pre>

<br></br>


Check if the packet can be accepted. It depends if ID is whitelisted,
and the Max packets can be accepted in the Period seconds.
<a name="init-1"></a>

### init/1 ###


<pre><code>
init(Whitelist::[binary()]) -&gt; ok
</code></pre>

<br></br>


Init the monitor. Adds the JIDs to the whitelist.
