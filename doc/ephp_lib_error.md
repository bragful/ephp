

# Module ephp_lib_error #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_func`](ephp_func.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#debug_backtrace-4">debug_backtrace/4</a></td><td></td></tr><tr><td valign="top"><a href="#debug_print_backtrace-4">debug_print_backtrace/4</a></td><td></td></tr><tr><td valign="top"><a href="#error_clear_last-2">error_clear_last/2</a></td><td></td></tr><tr><td valign="top"><a href="#error_get_last-2">error_get_last/2</a></td><td></td></tr><tr><td valign="top"><a href="#error_reporting-3">error_reporting/3</a></td><td></td></tr><tr><td valign="top"><a href="#init_config-0">init_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_const-0">init_const/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#restore_error_handler-2">restore_error_handler/2</a></td><td></td></tr><tr><td valign="top"><a href="#restore_exception_handler-2">restore_exception_handler/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_error_handler-4">set_error_handler/4</a></td><td></td></tr><tr><td valign="top"><a href="#set_exception_handler-3">set_exception_handler/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="debug_backtrace-4"></a>

### debug_backtrace/4 ###

<pre><code>
debug_backtrace(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="debug_print_backtrace-4"></a>

### debug_print_backtrace/4 ###

<pre><code>
debug_print_backtrace(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; undefined
</code></pre>
<br />

<a name="error_clear_last-2"></a>

### error_clear_last/2 ###

<pre><code>
error_clear_last(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; undefined
</code></pre>
<br />

<a name="error_get_last-2"></a>

### error_get_last/2 ###

<pre><code>
error_get_last(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a> | undefined
</code></pre>
<br />

<a name="error_reporting-3"></a>

### error_reporting/3 ###

<pre><code>
error_reporting(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; integer()
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

<a name="restore_error_handler-2"></a>

### restore_error_handler/2 ###

<pre><code>
restore_error_handler(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; true
</code></pre>
<br />

<a name="restore_exception_handler-2"></a>

### restore_exception_handler/2 ###

<pre><code>
restore_exception_handler(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; true
</code></pre>
<br />

<a name="set_error_handler-4"></a>

### set_error_handler/4 ###

<pre><code>
set_error_handler(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-mixed">mixed()</a>
</code></pre>
<br />

<a name="set_exception_handler-3"></a>

### set_exception_handler/3 ###

<pre><code>
set_exception_handler(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-callable">callable()</a>
</code></pre>
<br />

