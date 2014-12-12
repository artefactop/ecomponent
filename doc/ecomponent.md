

# Module ecomponent #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#gen_id-0">gen_id/0</a></td><td>generate an ID based on UUID v4.</td></tr><tr><td valign="top"><a href="#prepare_id-1">prepare_id/1</a></td><td>Ensure the string used as ID in stanza is XML valid.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send an IQ stanza.</td></tr><tr><td valign="top"><a href="#send-3">send/3</a></td><td>Send an IQ stanza and set the return path.</td></tr><tr><td valign="top"><a href="#send-4">send/4</a></td><td>Send an IQ stanza, set the return path and if there are reply or not.</td></tr><tr><td valign="top"><a href="#send-5">send/5</a></td><td>Send an IQ stanza with return path, reply and the server where to send.</td></tr><tr><td valign="top"><a href="#send_message-1">send_message/1</a></td><td>Send a message stanza.</td></tr><tr><td valign="top"><a href="#send_message-2">send_message/2</a></td><td>Send a message stanza.</td></tr><tr><td valign="top"><a href="#send_presence-1">send_presence/1</a></td><td>Send a presence stanza.</td></tr><tr><td valign="top"><a href="#send_presence-2">send_presence/2</a></td><td>Send a presence stanza.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Starts the ecomponent main server.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stops the ecomponent main server.</td></tr><tr><td valign="top"><a href="#sync_send-2">sync_send/2</a></td><td>Send a packet and wait for the reply.</td></tr><tr><td valign="top"><a href="#sync_send-3">sync_send/3</a></td><td>Send a packet and wait for the reply using a specific server to send.</td></tr><tr><td valign="top"><a href="#sync_send-4">sync_send/4</a></td><td>Send a packet and wait for the reply using a specific server to send
and the time to await.</td></tr><tr><td valign="top"><a href="#unprepare_id-1">unprepare_id/1</a></td><td>Undo de action of prepare_id/1.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="gen_id-0"></a>

### gen_id/0 ###


<pre><code>
gen_id() -&gt; binary()
</code></pre>

<br></br>


generate an ID based on UUID v4.
<a name="prepare_id-1"></a>

### prepare_id/1 ###


<pre><code>
prepare_id(Data::string()) -&gt; string()
</code></pre>

<br></br>


Ensure the string used as ID in stanza is XML valid.
<a name="send-2"></a>

### send/2 ###


<pre><code>
send(Packet::term(), App::atom()) -&gt; ok
</code></pre>

<br></br>


Send an IQ stanza. The first param is a Packet in exmpp_xml format.
The second param is the app where the IQ is linked, this could be an
atom or a PID.
<a name="send-3"></a>

### send/3 ###


<pre><code>
send(Packet::term(), NS::atom(), App::atom()) -&gt; ok
</code></pre>

<br></br>


Send an IQ stanza and set the return path. The same as send/2 but adds
a middle param, the NS (namespace) referer to the name space of the
first element inside the iq stanza.
<a name="send-4"></a>

### send/4 ###


<pre><code>
send(Packet::term(), NS::atom(), App::atom(), Reply::boolean()) -&gt; ok
</code></pre>

<br></br>


Send an IQ stanza, set the return path and if there are reply or not.
This function is the same as send/3 but adds a new param. The fourth
param is for enable or disable the waiting for the reply.
<a name="send-5"></a>

### send/5 ###


<pre><code>
send(Packet::term(), NS::atom(), App::atom(), Reply::boolean(), ServerID::atom()) -&gt; ok
</code></pre>

<br></br>


Send an IQ stanza with return path, reply and the server where to send.
This function adds a new param. The fifth param is the name of the
connection where the stanza should be sent.
<a name="send_message-1"></a>

### send_message/1 ###


<pre><code>
send_message(Packet::term()) -&gt; ok
</code></pre>

<br></br>


Send a message stanza. The Packet should be in exmpp_xml format.
<a name="send_message-2"></a>

### send_message/2 ###


<pre><code>
send_message(Packet::term(), ServerID::atom()) -&gt; ok
</code></pre>

<br></br>


Send a message stanza. The Packet should be in exmpp_xml format.
This function adds the possibility to select the server for send
the stanza.
<a name="send_presence-1"></a>

### send_presence/1 ###


<pre><code>
send_presence(Packet::term()) -&gt; ok
</code></pre>

<br></br>


Send a presence stanza. The Packet should be in exmpp_xml format.
<a name="send_presence-2"></a>

### send_presence/2 ###


<pre><code>
send_presence(Packet::term(), ServerID::atom()) -&gt; ok
</code></pre>

<br></br>


Send a presence stanza. The Packet should be in exmpp_xml format.
This function adds the possibility to select the server for send
the stanza.
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
<a name="sync_send-2"></a>

### sync_send/2 ###


<pre><code>
sync_send(Packet::term(), NS::atom()) -&gt; #params{} | {error, timeout}
</code></pre>

<br></br>


Send a packet and wait for the reply.
Send a packet as do send/3, but the return path is the current process
for get the response. The first param is a Packet in the exmpp_xml format.
The second param is the namespace (NS).
<a name="sync_send-3"></a>

### sync_send/3 ###


<pre><code>
sync_send(Packet::term(), NS::atom(), ServerID::atom()) -&gt; #params{} | {error, timeout}
</code></pre>

<br></br>


Send a packet and wait for the reply using a specific server to send.
As in send/3, but the return path is the current process for get the
response.
<a name="sync_send-4"></a>

### sync_send/4 ###


<pre><code>
sync_send(Packet::term(), NS::atom(), ServerID::atom(), Timeout::pos_integer()) -&gt; #params{} | {error, tiemout}
</code></pre>

<br></br>


Send a packet and wait for the reply using a specific server to send
and the time to await. As in send/3, but you can specify the timeout
in milliseconds.
<a name="unprepare_id-1"></a>

### unprepare_id/1 ###


<pre><code>
unprepare_id(Data::string()) -&gt; string()
</code></pre>

<br></br>


Undo de action of prepare_id/1.
