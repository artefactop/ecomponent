

# Module ecomponent_con_worker #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###


<pre><code>
code_change(OldVsn::string(), State::#state{}, Extra::any()) -&gt; {ok, State::#state{}}
</code></pre>

<br></br>



<a name="handle_call-3"></a>

### handle_call/3 ###


<pre><code>
handle_call(Msg::any(), From::{pid(), term()}, State::#state{}) -&gt; {reply, Reply::any(), State::#state{}} | {reply, Reply::any(), State::#state{}, hibernate | infinity | non_neg_integer()} | {noreply, State::#state{}} | {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} | {stop, Reason::any(), Reply::any(), State::#state{}} | {stop, Reason::any(), State::#state{}}
</code></pre>

<br></br>



<a name="handle_cast-2"></a>

### handle_cast/2 ###


<pre><code>
handle_cast(Msg::any(), State::#state{}) -&gt; {noreply, State::#state{}} | {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} | {stop, Reason::any(), State::#state{}}
</code></pre>

<br></br>



<a name="handle_info-2"></a>

### handle_info/2 ###


<pre><code>
handle_info(Msg::any(), State::#state{}) -&gt; {noreply, State::#state{}} | {noreply, State::#state{}, hibernate | infinity | non_neg_integer()} | {stop, Reason::any(), State::#state{}}
</code></pre>

<br></br>



<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`


<a name="start_link-3"></a>

### start_link/3 ###

`start_link(ID, JID, Conf) -> any()`


<a name="stop-1"></a>

### stop/1 ###


<pre><code>
stop(ID::atom()) -&gt; ok
</code></pre>

<br></br>



<a name="terminate-2"></a>

### terminate/2 ###


<pre><code>
terminate(Reason::any(), State::#state{}) -&gt; ok
</code></pre>

<br></br>



