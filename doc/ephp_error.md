

# Module ephp_error #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-error_type">error_type()</a> ###


<pre><code>
error_type() = atom()
</code></pre>




### <a name="type-get_return_return">get_return_return()</a> ###


<pre><code>
get_return_return() = {ok, undefined} | {return, undefined}
</code></pre>




### <a name="type-throw_error">throw_error()</a> ###


<pre><code>
throw_error() = atom() | {error, <a href="#type-error_type">error_type()</a>, <a href="#type-line">line()</a>, <a href="#type-error_level">error_level()</a>, any()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_message_handler-2">add_message_handler/2</a></td><td></td></tr><tr><td valign="top"><a href="#clear_last-1">clear_last/1</a></td><td></td></tr><tr><td valign="top"><a href="#destroy-1">destroy/1</a></td><td></td></tr><tr><td valign="top"><a href="#error-1">error/1</a></td><td></td></tr><tr><td valign="top"><a href="#error_reporting-2">error_reporting/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_error_handler_func-1">get_error_handler_func/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_exception_handler_func-1">get_exception_handler_func/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_last-1">get_last/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_level-1">get_level/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_line-1">get_line/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_error-2">handle_error/2</a></td><td></td></tr><tr><td valign="top"><a href="#remove_error_handler_func-1">remove_error_handler_func/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove_exception_handler_func-1">remove_exception_handler_func/1</a></td><td></td></tr><tr><td valign="top"><a href="#run_quiet-2">run_quiet/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_error_handler_func-3">set_error_handler_func/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_exception_handler_func-2">set_exception_handler_func/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_output-2">set_output/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_output_handler-2">set_output_handler/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_message_handler-2"></a>

### add_message_handler/2 ###

<pre><code>
add_message_handler(Context::<a href="#type-context">context()</a>, Module::module()) -&gt; ok
</code></pre>
<br />

<a name="clear_last-1"></a>

### clear_last/1 ###

<pre><code>
clear_last(Context::<a href="#type-context">context()</a>) -&gt; ok
</code></pre>
<br />

<a name="destroy-1"></a>

### destroy/1 ###

<pre><code>
destroy(ErrorsId::<a href="ephp.md#type-errors_id">ephp:errors_id()</a>) -&gt; ok
</code></pre>
<br />

<a name="error-1"></a>

### error/1 ###

<pre><code>
error(X1::<a href="#type-throw_error">throw_error()</a>) -&gt; ok
</code></pre>
<br />

<a name="error_reporting-2"></a>

### error_reporting/2 ###

<pre><code>
error_reporting(Context::<a href="#type-context">context()</a>, Level::integer()) -&gt; integer()
</code></pre>
<br />

<a name="get_error_handler_func-1"></a>

### get_error_handler_func/1 ###

<pre><code>
get_error_handler_func(Context::<a href="#type-context">context()</a>) -&gt; {<a href="#type-callable">callable()</a>, <a href="#type-errorlevel">errorlevel()</a>} | undefined
</code></pre>
<br />

<a name="get_exception_handler_func-1"></a>

### get_exception_handler_func/1 ###

<pre><code>
get_exception_handler_func(Context::<a href="#type-context">context()</a>) -&gt; <a href="#type-callable">callable()</a> | undefined
</code></pre>
<br />

<a name="get_last-1"></a>

### get_last/1 ###

<pre><code>
get_last(Context::<a href="#type-context">context()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a> | undefined
</code></pre>
<br />

<a name="get_level-1"></a>

### get_level/1 ###

<pre><code>
get_level(Level::pos_integer()) -&gt; binary()
</code></pre>
<br />

<a name="get_line-1"></a>

### get_line/1 ###

<pre><code>
get_line(X1::<a href="#type-line">line()</a> | undefined) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

<a name="handle_error-2"></a>

### handle_error/2 ###

<pre><code>
handle_error(Context::<a href="#type-context">context()</a>, X2::{error, <a href="#type-error_type">error_type()</a>, <a href="#type-line">line()</a>, binary(), <a href="#type-error_level">error_level()</a>, any()}) -&gt; <a href="#type-get_return_return">get_return_return()</a>
</code></pre>
<br />

<a name="remove_error_handler_func-1"></a>

### remove_error_handler_func/1 ###

<pre><code>
remove_error_handler_func(Context::<a href="#type-context">context()</a>) -&gt; ok
</code></pre>
<br />

<a name="remove_exception_handler_func-1"></a>

### remove_exception_handler_func/1 ###

<pre><code>
remove_exception_handler_func(Context::<a href="#type-context">context()</a>) -&gt; ok
</code></pre>
<br />

<a name="run_quiet-2"></a>

### run_quiet/2 ###

<pre><code>
run_quiet(Errors::<a href="ephp.md#type-errors_id">ephp:errors_id()</a>, Fun::function()) -&gt; ok
</code></pre>
<br />

<a name="set_error_handler_func-3"></a>

### set_error_handler_func/3 ###

<pre><code>
set_error_handler_func(Context::<a href="#type-context">context()</a>, Callable::<a href="#type-callable">callable()</a>, ErrorLevel::<a href="#type-errorlevel">errorlevel()</a>) -&gt; ok
</code></pre>
<br />

<a name="set_exception_handler_func-2"></a>

### set_exception_handler_func/2 ###

<pre><code>
set_exception_handler_func(Context::<a href="#type-context">context()</a>, Callable::<a href="#type-callable">callable()</a>) -&gt; ok
</code></pre>
<br />

<a name="set_output-2"></a>

### set_output/2 ###

<pre><code>
set_output(Context::<a href="#type-context">context()</a>, Text::binary()) -&gt; ok
</code></pre>
<br />

<a name="set_output_handler-2"></a>

### set_output_handler/2 ###

<pre><code>
set_output_handler(Context::<a href="#type-context">context()</a>, Module::module()) -&gt; ok
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, <a href="ephp.md#type-errors_id">ephp:errors_id()</a>}
</code></pre>
<br />

