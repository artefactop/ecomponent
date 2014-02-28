

# Module ecomponent #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-levels">levels()</a> ###



<pre><code>
levels() = emerg | alert | crit | err | warning | notice | info | debug
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#configure-0">configure/0</a></td><td></td></tr><tr><td valign="top"><a href="#gen_id-0">gen_id/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_countdown-1">get_countdown/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_message_processor-0">get_message_processor/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_presence_processor-0">get_presence_processor/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_processor-1">get_processor/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_processor_by_ns-1">get_processor_by_ns/1</a></td><td></td></tr><tr><td valign="top"><a href="#prepare_id-1">prepare_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#reset_countdown-1">reset_countdown/1</a></td><td></td></tr><tr><td valign="top"><a href="#save_id-4">save_id/4</a></td><td></td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td></td></tr><tr><td valign="top"><a href="#send-3">send/3</a></td><td></td></tr><tr><td valign="top"><a href="#send-4">send/4</a></td><td></td></tr><tr><td valign="top"><a href="#send-5">send/5</a></td><td></td></tr><tr><td valign="top"><a href="#send_message-1">send_message/1</a></td><td></td></tr><tr><td valign="top"><a href="#send_message-2">send_message/2</a></td><td></td></tr><tr><td valign="top"><a href="#send_presence-1">send_presence/1</a></td><td></td></tr><tr><td valign="top"><a href="#send_presence-2">send_presence/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr><tr><td valign="top"><a href="#sync_send-2">sync_send/2</a></td><td></td></tr><tr><td valign="top"><a href="#sync_send-3">sync_send/3</a></td><td></td></tr><tr><td valign="top"><a href="#syslog-2">syslog/2</a></td><td></td></tr><tr><td valign="top"><a href="#unprepare_id-1">unprepare_id/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="configure-0"></a>

### configure/0 ###


<pre><code>
configure() -&gt; {ok, #state{}}
</code></pre>

<br></br>



<a name="gen_id-0"></a>

### gen_id/0 ###


<pre><code>
gen_id() -&gt; binary()
</code></pre>

<br></br>



<a name="get_countdown-1"></a>

### get_countdown/1 ###


<pre><code>
get_countdown(Begin::integer()) -&gt; integer()
</code></pre>

<br></br>



<a name="get_message_processor-0"></a>

### get_message_processor/0 ###


<pre><code>
get_message_processor() -&gt; undefined | <a href="#type-mod_processor">mod_processor()</a> | <a href="#type-app_processor">app_processor()</a>
</code></pre>

<br></br>



<a name="get_presence_processor-0"></a>

### get_presence_processor/0 ###


<pre><code>
get_presence_processor() -&gt; undefined | <a href="#type-mod_processor">mod_processor()</a> | <a href="#type-app_processor">app_processor()</a>
</code></pre>

<br></br>



<a name="get_processor-1"></a>

### get_processor/1 ###


<pre><code>
get_processor(Id::binary()) -&gt; #matching{} | undefined
</code></pre>

<br></br>



<a name="get_processor_by_ns-1"></a>

### get_processor_by_ns/1 ###


<pre><code>
get_processor_by_ns(NS::atom()) -&gt; [] | <a href="#type-mod_processor">mod_processor()</a> | <a href="#type-app_processor">app_processor()</a>
</code></pre>

<br></br>



<a name="prepare_id-1"></a>

### prepare_id/1 ###


<pre><code>
prepare_id(Data::string()) -&gt; string()
</code></pre>

<br></br>



<a name="reset_countdown-1"></a>

### reset_countdown/1 ###


<pre><code>
reset_countdown(State::#state{}) -&gt; #state{}
</code></pre>

<br></br>



<a name="save_id-4"></a>

### save_id/4 ###


<pre><code>
save_id(Id::binary(), NS::string(), Packet::term(), App::atom()) -&gt; #matching{} | ok
</code></pre>

<br></br>



<a name="send-2"></a>

### send/2 ###


<pre><code>
send(Packet::term(), App::atom()) -&gt; ok
</code></pre>

<br></br>



<a name="send-3"></a>

### send/3 ###


<pre><code>
send(Packet::term(), NS::atom(), App::atom()) -&gt; ok
</code></pre>

<br></br>



<a name="send-4"></a>

### send/4 ###


<pre><code>
send(Packet::term(), NS::atom(), App::atom(), Reply::boolean()) -&gt; ok
</code></pre>

<br></br>



<a name="send-5"></a>

### send/5 ###


<pre><code>
send(Packet::term(), NS::atom(), App::atom(), Reply::boolean(), ServerID::atom()) -&gt; ok
</code></pre>

<br></br>



<a name="send_message-1"></a>

### send_message/1 ###


<pre><code>
send_message(Packet::term()) -&gt; ok
</code></pre>

<br></br>



<a name="send_message-2"></a>

### send_message/2 ###


<pre><code>
send_message(Packet::term(), ServerID::atom()) -&gt; ok
</code></pre>

<br></br>



<a name="send_presence-1"></a>

### send_presence/1 ###


<pre><code>
send_presence(Packet::term()) -&gt; ok
</code></pre>

<br></br>



<a name="send_presence-2"></a>

### send_presence/2 ###


<pre><code>
send_presence(Packet::term(), ServerID::atom()) -&gt; ok
</code></pre>

<br></br>



<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, Pid::pid()} | {error, Reason::any()}
</code></pre>

<br></br>



<a name="stop-0"></a>

### stop/0 ###


<pre><code>
stop() -&gt; ok
</code></pre>

<br></br>



<a name="sync_send-2"></a>

### sync_send/2 ###


<pre><code>
sync_send(Packet::term(), NS::atom()) -&gt; #params{} | {error, timeout}
</code></pre>

<br></br>



<a name="sync_send-3"></a>

### sync_send/3 ###


<pre><code>
sync_send(Packet::term(), NS::atom(), ServerID::atom()) -&gt; #params{} | {error, timeout}
</code></pre>

<br></br>



<a name="syslog-2"></a>

### syslog/2 ###


<pre><code>
syslog(Level::<a href="#type-levels">levels()</a>, Message::string()) -&gt; ok
</code></pre>

<br></br>



<a name="unprepare_id-1"></a>

### unprepare_id/1 ###


<pre><code>
unprepare_id(Data::string()) -&gt; string()
</code></pre>

<br></br>



