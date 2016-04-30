

# Module ephp_lib_ob #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_func`](ephp_func.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#flush-2">flush/2</a></td><td></td></tr><tr><td valign="top"><a href="#init_config-0">init_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#ob_clean-2">ob_clean/2</a></td><td></td></tr><tr><td valign="top"><a href="#ob_get_contents-2">ob_get_contents/2</a></td><td></td></tr><tr><td valign="top"><a href="#ob_get_length-2">ob_get_length/2</a></td><td></td></tr><tr><td valign="top"><a href="#ob_start-2">ob_start/2</a></td><td></td></tr><tr><td valign="top"><a href="#ob_start-3">ob_start/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="flush-2"></a>

### flush/2 ###

<pre><code>
flush(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; undefined
</code></pre>
<br />

<a name="init_config-0"></a>

### init_config/0 ###

<pre><code>
init_config() -&gt; <a href="ephp_func.md#type-php_config_results">ephp_func:php_config_results()</a>
</code></pre>
<br />

<a name="init_func-0"></a>

### init_func/0 ###

<pre><code>
init_func() -&gt; <a href="ephp_func.md#type-php_function_results">ephp_func:php_function_results()</a>
</code></pre>
<br />

<a name="ob_clean-2"></a>

### ob_clean/2 ###

<pre><code>
ob_clean(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; undefined
</code></pre>
<br />

<a name="ob_get_contents-2"></a>

### ob_get_contents/2 ###

<pre><code>
ob_get_contents(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; binary()
</code></pre>
<br />

<a name="ob_get_length-2"></a>

### ob_get_length/2 ###

<pre><code>
ob_get_length(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; integer()
</code></pre>
<br />

<a name="ob_start-2"></a>

### ob_start/2 ###

<pre><code>
ob_start(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="ob_start-3"></a>

### ob_start/3 ###

<pre><code>
ob_start(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Callback::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

