

# Module ephp_util #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#increment_code-1">increment_code/1</a></td><td></td></tr><tr><td valign="top"><a href="#pad_to_bin-2">pad_to_bin/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_bin-1">to_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_bool-1">to_bool/1</a></td><td></td></tr><tr><td valign="top"><a href="#zero_if_undef-1">zero_if_undef/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

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


<a name="to_bool-1"></a>

### to_bool/1 ###


<pre><code>
to_bool(Value::undefined | dict() | integer() | float() | string() | binary()) -&gt; boolean()
</code></pre>
<br />


<a name="zero_if_undef-1"></a>

### zero_if_undef/1 ###


<pre><code>
zero_if_undef(Value::undefined | dict() | integer() | float() | string() | binary()) -&gt; integer()
</code></pre>
<br />


