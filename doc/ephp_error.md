

# Module ephp_error #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-error_format">error_format()</a> ###


<pre><code>
error_format() = text | html
</code></pre>




### <a name="type-error_type">error_type()</a> ###


<pre><code>
error_type() = eundefclass | eprivateaccess | ecallprivate | eunsupportop | erequired | einclude | enofile | eundefun | earrayundef | eundeftoken | enoclassscope | emethodtypes | eundefmethod | edivzero | eparse | enostatement | eunknownst | eundefvar | eundefconst | eassignthis | earrayconv
</code></pre>




### <a name="type-throw_error">throw_error()</a> ###


<pre><code>
throw_error() = atom() | {error, <a href="#type-error_type">error_type()</a>, <a href="#type-line">line()</a>, <a href="#type-error_level">error_level()</a>, any()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_message_handler-2">add_message_handler/2</a></td><td></td></tr><tr><td valign="top"><a href="#destroy-1">destroy/1</a></td><td></td></tr><tr><td valign="top"><a href="#error-1">error/1</a></td><td></td></tr><tr><td valign="top"><a href="#error_reporting-2">error_reporting/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_level-1">get_level/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_line-1">get_line/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_error-2">handle_error/2</a></td><td></td></tr><tr><td valign="top"><a href="#run_quiet-2">run_quiet/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_error_format-2">set_error_format/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_output-2">set_output/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_output_handler-2">set_output_handler/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_message_handler-2"></a>

### add_message_handler/2 ###

<pre><code>
add_message_handler(Context::<a href="ephp.md#type-context">ephp:context()</a>, Module::module()) -&gt; ok
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
handle_error(Context::<a href="#type-context">context()</a>, X2::{error, <a href="#type-error_type">error_type()</a>, <a href="#type-line">line()</a>, binary(), <a href="#type-error_level">error_level()</a>, any()}) -&gt; ok
</code></pre>
<br />

<a name="run_quiet-2"></a>

### run_quiet/2 ###

<pre><code>
run_quiet(Errors::<a href="ephp.md#type-errors_id">ephp:errors_id()</a>, Fun::function()) -&gt; ok
</code></pre>
<br />

<a name="set_error_format-2"></a>

### set_error_format/2 ###

<pre><code>
set_error_format(Context::<a href="#type-context">context()</a>, Format::<a href="#type-error_format">error_format()</a>) -&gt; ok
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

