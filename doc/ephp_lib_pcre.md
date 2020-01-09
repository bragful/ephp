

# Module ephp_lib_pcre #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_lib`](ephp_lib.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init_config-0">init_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_const-0">init_const/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#preg_match-7">preg_match/7</a></td><td></td></tr><tr><td valign="top"><a href="#preg_quote-4">preg_quote/4</a></td><td></td></tr><tr><td valign="top"><a href="#preg_replace-7">preg_replace/7</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init_config-0"></a>

### init_config/0 ###

<pre><code>
init_config() -&gt; <a href="ephp_lib.md#type-php_config_results">ephp_lib:php_config_results()</a>
</code></pre>
<br />

<a name="init_const-0"></a>

### init_const/0 ###

<pre><code>
init_const() -&gt; <a href="ephp_lib.md#type-php_const_results">ephp_lib:php_const_results()</a>
</code></pre>
<br />

<a name="init_func-0"></a>

### init_func/0 ###

<pre><code>
init_func() -&gt; <a href="ephp_lib.md#type-php_function_results">ephp_lib:php_function_results()</a>
</code></pre>
<br />

<a name="preg_match-7"></a>

### preg_match/7 ###

<pre><code>
preg_match(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Pattern::<a href="#type-var_value">var_value()</a>, Subject::<a href="#type-var_value">var_value()</a>, Matches::<a href="#type-var_value">var_value()</a>, Flags::<a href="#type-var_value">var_value()</a>, Offset::<a href="#type-var_value">var_value()</a>) -&gt; pos_integer()
</code></pre>
<br />

<a name="preg_quote-4"></a>

### preg_quote/4 ###

<pre><code>
preg_quote(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Str::<a href="#type-var_value">var_value()</a>, Delim::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="preg_replace-7"></a>

### preg_replace/7 ###

<pre><code>
preg_replace(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Pattern::<a href="#type-var_value">var_value()</a>, Replacement::<a href="#type-var_value">var_value()</a>, Subject::<a href="#type-var_value">var_value()</a>, Limit::<a href="#type-var_value">var_value()</a>, Count::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

