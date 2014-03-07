

# Module processor #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


The processor module is used when a processor is not found.

<a name="description"></a>

## Description ##
     This module is used when a processor is not found. The stanza
is sent to this module and the module report to the logs or
reply to the stanza with a 'feature-not-implemented'.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#process_iq-1">process_iq/1</a></td><td>Process the IQ stanza.</td></tr><tr><td valign="top"><a href="#process_message-1">process_message/1</a></td><td>Process the message stanza.</td></tr><tr><td valign="top"><a href="#process_presence-1">process_presence/1</a></td><td>Process the presence stanza.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="process_iq-1"></a>

### process_iq/1 ###


<pre><code>
process_iq(Params::#params{}) -&gt; ok
</code></pre>

<br></br>


Process the IQ stanza. Returns a feature-not-implemented error.
<a name="process_message-1"></a>

### process_message/1 ###


<pre><code>
process_message(Message::#message{}) -&gt; ok
</code></pre>

<br></br>


Process the message stanza. Drops the message.
<a name="process_presence-1"></a>

### process_presence/1 ###


<pre><code>
process_presence(Presence::#presence{}) -&gt; ok
</code></pre>

<br></br>


Process the presence stanza. Drops the message.
