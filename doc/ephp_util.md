

# Module ephp_util #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_abbr_month-1">get_abbr_month/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_abbr_weekday-1">get_abbr_weekday/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_line-1">get_line/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_month-1">get_month/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_timestamp-1">get_timestamp/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_weekday-1">get_weekday/1</a></td><td></td></tr><tr><td valign="top"><a href="#gettype-1">gettype/1</a></td><td></td></tr><tr><td valign="top"><a href="#increment_code-1">increment_code/1</a></td><td></td></tr><tr><td valign="top"><a href="#pad_to_bin-2">pad_to_bin/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_bin-1">to_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_bin-3">to_bin/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_bool-1">to_bool/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td></td></tr><tr><td valign="top"><a href="#zero_if_undef-1">zero_if_undef/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_abbr_month-1"></a>

### get_abbr_month/1 ###

<pre><code>
get_abbr_month(M::integer()) -&gt; binary()
</code></pre>
<br />

<a name="get_abbr_weekday-1"></a>

### get_abbr_weekday/1 ###

<pre><code>
get_abbr_weekday(D::<a href="#type-date">date()</a>) -&gt; binary()
</code></pre>
<br />

<a name="get_line-1"></a>

### get_line/1 ###

<pre><code>
get_line(X1::<a href="#type-line">line()</a> | undefined) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

<a name="get_month-1"></a>

### get_month/1 ###

<pre><code>
get_month(M::integer()) -&gt; binary()
</code></pre>
<br />

<a name="get_timestamp-1"></a>

### get_timestamp/1 ###

<pre><code>
get_timestamp(TS::integer() | float()) -&gt; <a href="timer.md#type-timestamp">timer:timestamp()</a>
</code></pre>
<br />

<a name="get_weekday-1"></a>

### get_weekday/1 ###

<pre><code>
get_weekday(D::<a href="#type-date">date()</a>) -&gt; binary()
</code></pre>
<br />

<a name="gettype-1"></a>

### gettype/1 ###

<pre><code>
gettype(Value::<a href="#type-mixed">mixed()</a>) -&gt; binary()
</code></pre>
<br />

<a name="increment_code-1"></a>

### increment_code/1 ###

<pre><code>
increment_code(Code::binary()) -&gt; integer() | binary()
</code></pre>
<br />

<a name="pad_to_bin-2"></a>

### pad_to_bin/2 ###

<pre><code>
pad_to_bin(Num::integer() | binary(), Pad::integer()) -&gt; binary()
</code></pre>
<br />

<a name="to_bin-1"></a>

### to_bin/1 ###

<pre><code>
to_bin(A::binary() | string() | integer() | undefined) -&gt; binary()
</code></pre>
<br />

<a name="to_bin-3"></a>

### to_bin/3 ###

<pre><code>
to_bin(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, A::binary() | string() | integer() | undefined) -&gt; binary()
</code></pre>
<br />

<a name="to_bool-1"></a>

### to_bool/1 ###

<pre><code>
to_bool(Value::undefined | boolean() | <a href="#type-ephp_array">ephp_array()</a> | integer() | float() | string() | binary()) -&gt; boolean()
</code></pre>
<br />

<a name="to_lower-1"></a>

### to_lower/1 ###

<pre><code>
to_lower(Text::binary()) -&gt; binary()
</code></pre>
<br />

<a name="zero_if_undef-1"></a>

### zero_if_undef/1 ###

<pre><code>
zero_if_undef(Value::undefined | <a href="#type-ephp_array">ephp_array()</a> | integer() | float() | string() | binary()) -&gt; integer()
</code></pre>
<br />

