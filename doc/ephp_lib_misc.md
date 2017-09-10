

# Module ephp_lib_misc #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_func`](ephp_func.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#define-4">define/4</a></td><td></td></tr><tr><td valign="top"><a href="#defined-3">defined/3</a></td><td></td></tr><tr><td valign="top"><a href="#exit-3">exit/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_error-3">handle_error/3</a></td><td></td></tr><tr><td valign="top"><a href="#init_config-0">init_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_const-0">init_const/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#pack-3">pack/3</a></td><td></td></tr><tr><td valign="top"><a href="#sleep-3">sleep/3</a></td><td></td></tr><tr><td valign="top"><a href="#unpack-4">unpack/4</a></td><td></td></tr><tr><td valign="top"><a href="#usleep-3">usleep/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="define-4"></a>

### define/4 ###

<pre><code>
define(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Constant::<a href="#type-var_value">var_value()</a>, Content::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="defined-3"></a>

### defined/3 ###

<pre><code>
defined(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="exit-3"></a>

### exit/3 ###

<pre><code>
exit(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Message::<a href="#type-var_value">var_value()</a>) -&gt; undefined
</code></pre>
<br />

<a name="handle_error-3"></a>

### handle_error/3 ###

<pre><code>
handle_error(Type::<a href="ephp_error.md#type-error_type">ephp_error:error_type()</a>, Level::<a href="ephp_error.md#type-error_level">ephp_error:error_level()</a>, Args::term()) -&gt; string() | ignore
</code></pre>
<br />

<a name="init_config-0"></a>

### init_config/0 ###

<pre><code>
init_config() -&gt; <a href="ephp_func.md#type-php_config_results">ephp_func:php_config_results()</a>
</code></pre>
<br />

<a name="init_const-0"></a>

### init_const/0 ###

<pre><code>
init_const() -&gt; <a href="ephp_func.md#type-php_const_results">ephp_func:php_const_results()</a>
</code></pre>
<br />

<a name="init_func-0"></a>

### init_func/0 ###

<pre><code>
init_func() -&gt; <a href="ephp_func.md#type-php_function_results">ephp_func:php_function_results()</a>
</code></pre>
<br />

<a name="pack-3"></a>

### pack/3 ###

<pre><code>
pack(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Args::[<a href="#type-var_value">var_value()</a>]) -&gt; binary()
</code></pre>
<br />

<a name="sleep-3"></a>

### sleep/3 ###

<pre><code>
sleep(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Seconds::<a href="#type-var_value">var_value()</a>) -&gt; false | integer()
</code></pre>
<br />

<a name="unpack-4"></a>

### unpack/4 ###

<pre><code>
unpack(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="usleep-3"></a>

### usleep/3 ###

<pre><code>
usleep(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, MicroSeconds::<a href="#type-var_value">var_value()</a>) -&gt; false | integer()
</code></pre>
<br />

