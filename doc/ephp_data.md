

# Module ephp_data #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bin_to_number-1">bin_to_number/1</a></td><td></td></tr><tr><td valign="top"><a href="#bin_to_number-2">bin_to_number/2</a></td><td></td></tr><tr><td valign="top"><a href="#ceiling-1">ceiling/1</a></td><td></td></tr><tr><td valign="top"><a href="#ceiling-1">ceiling/1</a></td><td></td></tr><tr><td valign="top"><a href="#flooring-1">flooring/1</a></td><td></td></tr><tr><td valign="top"><a href="#flooring-1">flooring/1</a></td><td></td></tr><tr><td valign="top"><a href="#gettype-1">gettype/1</a></td><td></td></tr><tr><td valign="top"><a href="#increment_code-1">increment_code/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_equal-2">is_equal/2</a></td><td></td></tr><tr><td valign="top"><a href="#pad_to_bin-2">pad_to_bin/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_bin-1">to_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_bin-3">to_bin/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_bool-1">to_bool/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_boolean-1">to_boolean/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_float-1">to_float/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_float-3">to_float/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_int-1">to_int/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_int-3">to_int/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_number-1">to_number/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_number-3">to_number/3</a></td><td></td></tr><tr><td valign="top"><a href="#zero_if_undef-1">zero_if_undef/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bin_to_number-1"></a>

### bin_to_number/1 ###

<pre><code>
bin_to_number(Bin::binary()) -&gt; integer() | float()
</code></pre>
<br />

<a name="bin_to_number-2"></a>

### bin_to_number/2 ###

<pre><code>
bin_to_number(Bin::binary(), Force::true) -&gt; integer() | float()
</code></pre>
<br />

<a name="ceiling-1"></a>

### ceiling/1 ###

<pre><code>
ceiling(X::number()) -&gt; integer()
</code></pre>
<br />

<a name="ceiling-1"></a>

### ceiling/1 ###

`ceiling(X) -> any()`

<a name="flooring-1"></a>

### flooring/1 ###

<pre><code>
flooring(X::number()) -&gt; integer()
</code></pre>
<br />

<a name="flooring-1"></a>

### flooring/1 ###

`flooring(X) -> any()`

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
to_bin(A::<a href="#type-mixed">mixed()</a>) -&gt; binary()
</code></pre>
<br />

<a name="to_bin-3"></a>

### to_bin/3 ###

<pre><code>
to_bin(Ctx::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-mixed">mixed()</a>) -&gt; binary()
</code></pre>
<br />

<a name="to_bool-1"></a>

### to_bool/1 ###

<pre><code>
to_bool(Value::undefined | boolean() | <a href="#type-ephp_array">ephp_array()</a> | integer() | float() | string() | binary()) -&gt; boolean()
</code></pre>
<br />

<a name="to_boolean-1"></a>

### to_boolean/1 ###

<pre><code>
to_boolean(N::<a href="#type-mixed">mixed()</a>) -&gt; binary()
</code></pre>
<br />

<a name="to_float-1"></a>

### to_float/1 ###

<pre><code>
to_float(I::<a href="#type-mixed">mixed()</a>) -&gt; float()
</code></pre>
<br />

<a name="to_float-3"></a>

### to_float/3 ###

<pre><code>
to_float(Ctx::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Obj_ref::<a href="#type-mixed">mixed()</a>) -&gt; float()
</code></pre>
<br />

<a name="to_int-1"></a>

### to_int/1 ###

<pre><code>
to_int(A::<a href="#type-mixed">mixed()</a>) -&gt; integer()
</code></pre>
<br />

<a name="to_int-3"></a>

### to_int/3 ###

<pre><code>
to_int(Ctx::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Obj_ref::<a href="#type-mixed">mixed()</a>) -&gt; integer()
</code></pre>
<br />

<a name="to_number-1"></a>

### to_number/1 ###

<pre><code>
to_number(I::<a href="#type-mixed">mixed()</a>) -&gt; float() | integer()
</code></pre>
<br />

<a name="to_number-3"></a>

### to_number/3 ###

<pre><code>
to_number(Ctx::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Obj_ref::<a href="#type-mixed">mixed()</a>) -&gt; float() | integer()
</code></pre>
<br />

<a name="zero_if_undef-1"></a>

### zero_if_undef/1 ###

<pre><code>
zero_if_undef(Value::undefined | <a href="#type-ephp_array">ephp_array()</a> | integer() | float() | string() | binary()) -&gt; integer()
</code></pre>
<br />

