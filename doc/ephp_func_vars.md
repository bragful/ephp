

# Module ephp_func_vars #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_func`](ephp_func.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#empty-3">empty/3</a></td><td></td></tr><tr><td valign="top"><a href="#gettype-3">gettype/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#isset-3">isset/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_is_array-3">php_is_array/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_is_bool-3">php_is_bool/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_is_float-3">php_is_float/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_is_integer-3">php_is_integer/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_is_null-3">php_is_null/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_is_numeric-3">php_is_numeric/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_is_object-3">php_is_object/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_is_string-3">php_is_string/3</a></td><td></td></tr><tr><td valign="top"><a href="#print_r-3">print_r/3</a></td><td></td></tr><tr><td valign="top"><a href="#print_r-4">print_r/4</a></td><td></td></tr><tr><td valign="top"><a href="#unset-3">unset/3</a></td><td></td></tr><tr><td valign="top"><a href="#var_dump-3">var_dump/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="empty-3"></a>

### empty/3 ###


<pre><code>
empty(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="gettype-3"></a>

### gettype/3 ###


<pre><code>
gettype(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />


<a name="init-0"></a>

### init/0 ###


<pre><code>
init() -&gt; [<a href="ephp_func.md#type-php_function">ephp_func:php_function()</a> | {<a href="ephp_func.md#type-php_function">ephp_func:php_function()</a>, <a href="ephp_func.md#type-php_function_alias">ephp_func:php_function_alias()</a>}]
</code></pre>
<br />


<a name="isset-3"></a>

### isset/3 ###


<pre><code>
isset(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="php_is_array-3"></a>

### php_is_array/3 ###


<pre><code>
php_is_array(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="php_is_bool-3"></a>

### php_is_bool/3 ###


<pre><code>
php_is_bool(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="php_is_float-3"></a>

### php_is_float/3 ###


<pre><code>
php_is_float(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="php_is_integer-3"></a>

### php_is_integer/3 ###


<pre><code>
php_is_integer(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="php_is_null-3"></a>

### php_is_null/3 ###


<pre><code>
php_is_null(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Var::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="php_is_numeric-3"></a>

### php_is_numeric/3 ###


<pre><code>
php_is_numeric(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="php_is_object-3"></a>

### php_is_object/3 ###


<pre><code>
php_is_object(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="php_is_string-3"></a>

### php_is_string/3 ###


<pre><code>
php_is_string(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="print_r-3"></a>

### print_r/3 ###


<pre><code>
print_r(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Vars::<a href="#type-var_value">var_value()</a>) -&gt; true | binary()
</code></pre>
<br />


<a name="print_r-4"></a>

### print_r/4 ###


<pre><code>
print_r(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Val::<a href="#type-var_value">var_value()</a>, Output::boolean()) -&gt; true | binary()
</code></pre>
<br />


<a name="unset-3"></a>

### unset/3 ###


<pre><code>
unset(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; null
</code></pre>
<br />


<a name="var_dump-3"></a>

### var_dump/3 ###


<pre><code>
var_dump(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; null
</code></pre>
<br />


