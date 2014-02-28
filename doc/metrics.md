

# Module metrics #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#notify-2">notify/2</a></td><td></td></tr><tr><td valign="top"><a href="#notify_dropped_iq-2">notify_dropped_iq/2</a></td><td></td></tr><tr><td valign="top"><a href="#notify_dropped_message-1">notify_dropped_message/1</a></td><td></td></tr><tr><td valign="top"><a href="#notify_dropped_presence-1">notify_dropped_presence/1</a></td><td></td></tr><tr><td valign="top"><a href="#notify_resp_time-1">notify_resp_time/1</a></td><td></td></tr><tr><td valign="top"><a href="#notify_throughput_iq-3">notify_throughput_iq/3</a></td><td></td></tr><tr><td valign="top"><a href="#notify_throughput_message-2">notify_throughput_message/2</a></td><td></td></tr><tr><td valign="top"><a href="#notify_throughput_presence-2">notify_throughput_presence/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_iq_time-3">set_iq_time/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-0"></a>

### init/0 ###


<pre><code>
init() -&gt; ok
</code></pre>

<br></br>



<a name="notify-2"></a>

### notify/2 ###


<pre><code>
notify(Name::string(), Value::integer()) -&gt; ok
</code></pre>

<br></br>



<a name="notify_dropped_iq-2"></a>

### notify_dropped_iq/2 ###


<pre><code>
notify_dropped_iq(Type::atom(), NS::atom()) -&gt; ok | {error, Name::atom(), nonexistent_metric} | {error, Type::atom(), unsupported_metric_type}
</code></pre>

<br></br>



<a name="notify_dropped_message-1"></a>

### notify_dropped_message/1 ###


<pre><code>
notify_dropped_message(Type::atom()) -&gt; ok | {error, Name::atom(), nonexistent_metric} | {error, Type::atom(), unsupported_metric_type}
</code></pre>

<br></br>



<a name="notify_dropped_presence-1"></a>

### notify_dropped_presence/1 ###


<pre><code>
notify_dropped_presence(Type::atom()) -&gt; ok | {error, Name::atom(), nonexistent_metric} | {error, Type::atom(), unsupported_metric_type}
</code></pre>

<br></br>



<a name="notify_resp_time-1"></a>

### notify_resp_time/1 ###


<pre><code>
notify_resp_time(Id::binary()) -&gt; ok | {error, Name::atom(), nonexistent_metric} | {error, Type::atom(), unsupported_metric_type}
</code></pre>

<br></br>



<a name="notify_throughput_iq-3"></a>

### notify_throughput_iq/3 ###


<pre><code>
notify_throughput_iq(IO::atom(), Type::atom(), NS::atom()) -&gt; ok | {error, Name::atom(), nonexistent_metric} | {error, Type::atom(), unsupported_metric_type}
</code></pre>

<br></br>



<a name="notify_throughput_message-2"></a>

### notify_throughput_message/2 ###


<pre><code>
notify_throughput_message(IO::atom(), Type::atom()) -&gt; ok | {error, Name::atom(), nonexistent_metric} | {error, Type::atom(), unsupported_metric_type}
</code></pre>

<br></br>



<a name="notify_throughput_presence-2"></a>

### notify_throughput_presence/2 ###


<pre><code>
notify_throughput_presence(IO::atom(), Type::atom()) -&gt; ok | {error, Name::atom(), nonexistent_metric} | {error, Type::atom(), unsupported_metric_type}
</code></pre>

<br></br>



<a name="set_iq_time-3"></a>

### set_iq_time/3 ###


<pre><code>
set_iq_time(Id::binary(), Type::atom(), NS::atom()) -&gt; boolean()
</code></pre>

<br></br>



