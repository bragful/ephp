

# Module ephp_lib_string #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_func`](ephp_func.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#chr-3">chr/3</a></td><td></td></tr><tr><td valign="top"><a href="#explode-4">explode/4</a></td><td></td></tr><tr><td valign="top"><a href="#explode-5">explode/5</a></td><td></td></tr><tr><td valign="top"><a href="#implode-3">implode/3</a></td><td></td></tr><tr><td valign="top"><a href="#implode-4">implode/4</a></td><td></td></tr><tr><td valign="top"><a href="#init_config-0">init_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_const-0">init_const/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#ord-3">ord/3</a></td><td></td></tr><tr><td valign="top"><a href="#str_replace-5">str_replace/5</a></td><td></td></tr><tr><td valign="top"><a href="#str_replace-6">str_replace/6</a></td><td></td></tr><tr><td valign="top"><a href="#str_split-3">str_split/3</a></td><td></td></tr><tr><td valign="top"><a href="#str_split-4">str_split/4</a></td><td></td></tr><tr><td valign="top"><a href="#strlen-3">strlen/3</a></td><td></td></tr><tr><td valign="top"><a href="#strtolower-3">strtolower/3</a></td><td></td></tr><tr><td valign="top"><a href="#strtoupper-3">strtoupper/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="chr-3"></a>

### chr/3 ###

<pre><code>
chr(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Integer::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="explode-4"></a>

### explode/4 ###

<pre><code>
explode(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Delimiter::<a href="#type-var_value">var_value()</a>, String::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="explode-5"></a>

### explode/5 ###

<pre><code>
explode(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Delimiter::<a href="#type-var_value">var_value()</a>, String::<a href="#type-var_value">var_value()</a>, Limit::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="implode-3"></a>

### implode/3 ###

<pre><code>
implode(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Pieces::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="implode-4"></a>

### implode/4 ###

<pre><code>
implode(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Glue::<a href="#type-var_value">var_value()</a>, Pieces::<a href="#type-var_value">var_value()</a>) -&gt; binary()
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

<a name="ord-3"></a>

### ord/3 ###

<pre><code>
ord(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, String::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="str_replace-5"></a>

### str_replace/5 ###

<pre><code>
str_replace(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Search::<a href="#type-var_value">var_value()</a>, Replace::<a href="#type-var_value">var_value()</a>, Subject::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="str_replace-6"></a>

### str_replace/6 ###

<pre><code>
str_replace(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Search::<a href="#type-var_value">var_value()</a>, Replace::<a href="#type-var_value">var_value()</a>, Subject::<a href="#type-var_value">var_value()</a>, Count::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="str_split-3"></a>

### str_split/3 ###

<pre><code>
str_split(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Text::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="str_split-4"></a>

### str_split/4 ###

<pre><code>
str_split(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Text::<a href="#type-var_value">var_value()</a>, Size::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a> | undefined
</code></pre>
<br />

<a name="strlen-3"></a>

### strlen/3 ###

<pre><code>
strlen(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, String::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="strtolower-3"></a>

### strtolower/3 ###

<pre><code>
strtolower(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Text::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="strtoupper-3"></a>

### strtoupper/3 ###

<pre><code>
strtoupper(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Text::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

