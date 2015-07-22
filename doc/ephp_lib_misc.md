

# Module ephp_lib_misc #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_func`](ephp_func.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#define-4">define/4</a></td><td></td></tr><tr><td valign="top"><a href="#exit-3">exit/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#sleep-3">sleep/3</a></td><td></td></tr><tr><td valign="top"><a href="#usleep-3">usleep/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="define-4"></a>

### define/4 ###

<pre><code>
define(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Constant::<a href="#type-var_value">var_value()</a>, Content::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="exit-3"></a>

### exit/3 ###

<pre><code>
exit(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Message::<a href="#type-var_value">var_value()</a>) -&gt; null
</code></pre>
<br />

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; [<a href="ephp_func.md#type-php_function">ephp_func:php_function()</a>]
</code></pre>
<br />

<a name="sleep-3"></a>

### sleep/3 ###

<pre><code>
sleep(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Seconds::<a href="#type-var_value">var_value()</a>) -&gt; false | integer()
</code></pre>
<br />

<a name="usleep-3"></a>

### usleep/3 ###

<pre><code>
usleep(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, MicroSeconds::<a href="#type-var_value">var_value()</a>) -&gt; false | integer()
</code></pre>
<br />

