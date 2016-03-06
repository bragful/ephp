

# Module ephp_func_date #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_func`](ephp_func.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#date-3">date/3</a></td><td></td></tr><tr><td valign="top"><a href="#date-4">date/4</a></td><td></td></tr><tr><td valign="top"><a href="#date_default_timezone_get-2">date_default_timezone_get/2</a></td><td></td></tr><tr><td valign="top"><a href="#date_default_timezone_set-3">date_default_timezone_set/3</a></td><td></td></tr><tr><td valign="top"><a href="#gmdate-3">gmdate/3</a></td><td></td></tr><tr><td valign="top"><a href="#gmdate-4">gmdate/4</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#time-2">time/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="date-3"></a>

### date/3 ###

<pre><code>
date(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Format::{<a href="#type-variable">variable()</a>, binary()}) -&gt; binary()
</code></pre>
<br />

<a name="date-4"></a>

### date/4 ###

<pre><code>
date(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Format::{<a href="#type-variable">variable()</a>, binary()}, Timestamp::{<a href="#type-variable">variable()</a>, integer() | float()}) -&gt; binary()
</code></pre>
<br />

<a name="date_default_timezone_get-2"></a>

### date_default_timezone_get/2 ###

<pre><code>
date_default_timezone_get(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; binary()
</code></pre>
<br />

<a name="date_default_timezone_set-3"></a>

### date_default_timezone_set/3 ###

<pre><code>
date_default_timezone_set(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, TZ::{<a href="#type-variable">variable()</a>, binary()}) -&gt; binary()
</code></pre>
<br />

<a name="gmdate-3"></a>

### gmdate/3 ###

<pre><code>
gmdate(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Format::{<a href="#type-variable">variable()</a>, binary()}) -&gt; binary()
</code></pre>
<br />

<a name="gmdate-4"></a>

### gmdate/4 ###

<pre><code>
gmdate(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Format::{<a href="#type-variable">variable()</a>, binary()}, Timestamp::integer() | float()) -&gt; binary()
</code></pre>
<br />

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; [<a href="ephp_func.md#type-php_function">ephp_func:php_function()</a>]
</code></pre>
<br />

<a name="time-2"></a>

### time/2 ###

<pre><code>
time(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; integer()
</code></pre>
<br />

