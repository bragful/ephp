

# Module ephp_lib_math #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_func`](ephp_func.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init_config-0">init_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_consts-0">init_consts/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#php_abs-3">php_abs/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_acos-3">php_acos/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_asin-3">php_asin/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_ceil-3">php_ceil/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_floor-3">php_floor/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_round-3">php_round/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_sqrt-3">php_sqrt/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init_config-0"></a>

### init_config/0 ###

<pre><code>
init_config() -&gt; <a href="ephp_func.md#type-php_config_results">ephp_func:php_config_results()</a>
</code></pre>
<br />

<a name="init_consts-0"></a>

### init_consts/0 ###

<pre><code>
init_consts() -&gt; [{binary(), float() | integer() | binary()}]
</code></pre>
<br />

<a name="init_func-0"></a>

### init_func/0 ###

<pre><code>
init_func() -&gt; <a href="ephp_func.md#type-php_function_results">ephp_func:php_function_results()</a>
</code></pre>
<br />

<a name="php_abs-3"></a>

### php_abs/3 ###

<pre><code>
php_abs(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, NotANumber::number()) -&gt; number()
</code></pre>
<br />

<a name="php_acos-3"></a>

### php_acos/3 ###

<pre><code>
php_acos(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::number()) -&gt; float()
</code></pre>
<br />

<a name="php_asin-3"></a>

### php_asin/3 ###

<pre><code>
php_asin(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::number()) -&gt; float()
</code></pre>
<br />

<a name="php_ceil-3"></a>

### php_ceil/3 ###

<pre><code>
php_ceil(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="php_floor-3"></a>

### php_floor/3 ###

<pre><code>
php_floor(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="php_round-3"></a>

### php_round/3 ###

<pre><code>
php_round(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="php_sqrt-3"></a>

### php_sqrt/3 ###

<pre><code>
php_sqrt(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; float()
</code></pre>
<br />

