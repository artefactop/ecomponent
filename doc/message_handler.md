

# Module message_handler #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#pre_process_message-4">pre_process_message/4</a></td><td>Pre process the message stanza.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="pre_process_message-4"></a>

### pre_process_message/4 ###


<pre><code>
pre_process_message(Type::undefined | string(), Message::term(), From::<a href="ecomponent.md#type-jid">ecomponent:jid()</a>, ServerID::atom()) -&gt; ok
</code></pre>

<br></br>


Pre process the message stanza. If the message is an 'error' type
message should be addressed to the forward_response.
