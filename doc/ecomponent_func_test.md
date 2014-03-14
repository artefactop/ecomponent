

# Module ecomponent_func_test #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#check-1">check/1</a></td><td>Execute the suite for the list of tests passed as param.</td></tr><tr><td valign="top"><a href="#check-2">check/2</a></td><td>Execute the suite for the list of tests passed as param.</td></tr><tr><td valign="top"><a href="#check-3">check/3</a></td><td>Execute the suite for the list of tests passed as param.</td></tr><tr><td valign="top"><a href="#run-1">run/1</a></td><td>Run a single test.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="check-1"></a>

### check/1 ###


<pre><code>
check(Test::[string()]) -&gt; {timeout, integer(), function()}
</code></pre>

<br></br>


Execute the suite for the list of tests passed as param. This function
set the environment and run all the tests in the list.
<a name="check-2"></a>

### check/2 ###


<pre><code>
check(Test::[string()], Timeout::pos_integer()) -&gt; {timeout, integer(), function()}
</code></pre>

<br></br>


Execute the suite for the list of tests passed as param. This function
set the environment and run all the tests in the list. As second param
you can configure the timeout for the suite of tests.
<a name="check-3"></a>

### check/3 ###


<pre><code>
check(Test::[string()], Timeout::pos_integer(), Verbose::boolean()) -&gt; {timeout, integer(), function()}
</code></pre>

<br></br>


Execute the suite for the list of tests passed as param. This function
set the environment and run all the tests in the list. As second param
you can configure the timeout for the suite of tests and as third
param you can set if you want to show all the logs (lager and syslog)
or not.
<a name="run-1"></a>

### run/1 ###


<pre><code>
run(Test::string()) -&gt; ok
</code></pre>

<br></br>


Run a single test. This function runs a single test without prepare
the environment.
