

# Module ephp_data #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bin_to_number-1">bin_to_number/1</a></td><td></td></tr><tr><td valign="top"><a href="#gettype-1">gettype/1</a></td><td></td></tr><tr><td valign="top"><a href="#increment_code-1">increment_code/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_equal-2">is_equal/2</a></td><td></td></tr><tr><td valign="top"><a href="#pad_to_bin-2">pad_to_bin/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_bin-1">to_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_bin-3">to_bin/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_bool-1">to_bool/1</a></td><td></td></tr><tr><td valign="top"><a href="#zero_if_undef-1">zero_if_undef/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bin_to_number-1"></a>

### bin_to_number/1 ###

<pre><code>
bin_to_number(Bin::binary()) -&gt; integer() | float()
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

<a name="is_equal-2"></a>

### is_equal/2 ###

<pre><code>
is_equal(A::<a href="#type-mixed">mixed()</a>, B::<a href="#type-mixed">mixed()</a>) -&gt; boolean()
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

<a name="zero_if_undef-1"></a>

### zero_if_undef/1 ###

<pre><code>
zero_if_undef(Value::undefined | <a href="#type-ephp_array">ephp_array()</a> | integer() | float() | string() | binary()) -&gt; integer()
</code></pre>
<br />

