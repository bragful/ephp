

# Module ephp_lib_array #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_func`](ephp_func.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#array_change_key_case-4">array_change_key_case/4</a></td><td></td></tr><tr><td valign="top"><a href="#array_chunk-5">array_chunk/5</a></td><td></td></tr><tr><td valign="top"><a href="#array_column-5">array_column/5</a></td><td></td></tr><tr><td valign="top"><a href="#array_merge-3">array_merge/3</a></td><td></td></tr><tr><td valign="top"><a href="#array_unique-4">array_unique/4</a></td><td></td></tr><tr><td valign="top"><a href="#count-3">count/3</a></td><td></td></tr><tr><td valign="top"><a href="#in_array-5">in_array/5</a></td><td></td></tr><tr><td valign="top"><a href="#init_config-0">init_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_const-0">init_const/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#list-3">list/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="array_change_key_case-4"></a>

### array_change_key_case/4 ###

<pre><code>
array_change_key_case(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>, Flags::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="array_chunk-5"></a>

### array_chunk/5 ###

<pre><code>
array_chunk(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>, Size::<a href="#type-var_value">var_value()</a>, PreserveKeys::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="array_column-5"></a>

### array_column/5 ###

`array_column(Context, Line, Array, ColKey, IdxKey) -> any()`

<a name="array_merge-3"></a>

### array_merge/3 ###

<pre><code>
array_merge(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Arrays::[<a href="#type-var_value">var_value()</a>]) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="array_unique-4"></a>

### array_unique/4 ###

<pre><code>
array_unique(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>, Flags::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="count-3"></a>

### count/3 ###

<pre><code>
count(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="in_array-5"></a>

### in_array/5 ###

<pre><code>
in_array(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Key::<a href="#type-var_value">var_value()</a>, Array::<a href="#type-var_value">var_value()</a>, Strict::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
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

<a name="list-3"></a>

### list/3 ###

<pre><code>
list(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Vars::[<a href="#type-var_value">var_value()</a>]) -&gt; <a href="#type-ephp_array">ephp_array()</a> | undefined
</code></pre>
<br />

