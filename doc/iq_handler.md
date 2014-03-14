

# Module iq_handler #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#pre_process_iq-7">pre_process_iq/7</a></td><td>Pre-process the IQ stanza.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="pre_process_iq-7"></a>

### pre_process_iq/7 ###


<pre><code>
pre_process_iq(Type::undefined | string(), NS::atom(), IQ::term(), From::<a href="ecomponent.md#type-jid">ecomponent:jid()</a>, Features::[binary()], Info::<a href="proplists.md#type-proplists">proplists:proplists()</a>, ServerID::atom()) -&gt; ok
</code></pre>

<br></br>


Pre-process the IQ stanza. This process get more information from
the stanza and then send in a params record to the process_iq/1.
