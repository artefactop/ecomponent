

# Module timem #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-timem">timem()</a> ###



<pre><code>
timem() = {K::binary(), V::term()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#expired-1">expired/1</a></td><td></td></tr><tr><td valign="top"><a href="#insert-2">insert/2</a></td><td></td></tr><tr><td valign="top"><a href="#remove-1">remove/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove_expired-1">remove_expired/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="expired-1"></a>

### expired/1 ###


<pre><code>
expired(D::integer()) -&gt; [binary()]
</code></pre>

<br></br>



<a name="insert-2"></a>

### insert/2 ###


<pre><code>
insert(K::binary(), V::term()) -&gt; boolean()
</code></pre>

<br></br>



<a name="remove-1"></a>

### remove/1 ###


<pre><code>
remove(K::binary()) -&gt; <a href="#type-timem">timem()</a> | undefined
</code></pre>

<br></br>



<a name="remove_expired-1"></a>

### remove_expired/1 ###


<pre><code>
remove_expired(D::integer()) -&gt; [<a href="#type-timem">timem()</a>]
</code></pre>

<br></br>



