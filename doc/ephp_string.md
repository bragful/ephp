

# Module ephp_string #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bin2hex-1">bin2hex/1</a></td><td>transform a binary string in its hexadecimal representation.</td></tr><tr><td valign="top"><a href="#capitalize-2">capitalize/2</a></td><td></td></tr><tr><td valign="top"><a href="#escape-2">escape/2</a></td><td></td></tr><tr><td valign="top"><a href="#expand_mask-1">expand_mask/1</a></td><td></td></tr><tr><td valign="top"><a href="#hex2bin-1">hex2bin/1</a></td><td>transform a hexadecimal string in (little-endian).</td></tr><tr><td valign="top"><a href="#ibin2hex-1">ibin2hex/1</a></td><td>transform a binary string in its hexadecimal representation.</td></tr><tr><td valign="top"><a href="#ihex2bin-1">ihex2bin/1</a></td><td>transform a hexadecimal string in (big-endian).</td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td></td></tr><tr><td valign="top"><a href="#lpad-3">lpad/3</a></td><td></td></tr><tr><td valign="top"><a href="#ltrim-2">ltrim/2</a></td><td></td></tr><tr><td valign="top"><a href="#pad-3">pad/3</a></td><td></td></tr><tr><td valign="top"><a href="#repeat-2">repeat/2</a></td><td>repeat the byte passed as param as many times as the number passed as param.</td></tr><tr><td valign="top"><a href="#reverse-1">reverse/1</a></td><td>get the reverse of a string passed as param.</td></tr><tr><td valign="top"><a href="#rpad-3">rpad/3</a></td><td></td></tr><tr><td valign="top"><a href="#rtrim-2">rtrim/2</a></td><td></td></tr><tr><td valign="top"><a href="#spaces-1">spaces/1</a></td><td>generate as many spaces as the number passed as param.</td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_upper-1">to_upper/1</a></td><td></td></tr><tr><td valign="top"><a href="#trim-1">trim/1</a></td><td></td></tr><tr><td valign="top"><a href="#trim-2">trim/2</a></td><td></td></tr><tr><td valign="top"><a href="#vsn_cmp-2">vsn_cmp/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bin2hex-1"></a>

### bin2hex/1 ###

<pre><code>
bin2hex(Bin::binary()) -&gt; binary()
</code></pre>
<br />

transform a binary string in its hexadecimal representation.

<a name="capitalize-2"></a>

### capitalize/2 ###

<pre><code>
capitalize(X1::binary(), Sep::[byte()]) -&gt; binary()
</code></pre>
<br />

<a name="escape-2"></a>

### escape/2 ###

<pre><code>
escape(Bin::<a href="#type-mixed">mixed()</a>, Escape::non_neg_integer()) -&gt; binary()
</code></pre>
<br />

<a name="expand_mask-1"></a>

### expand_mask/1 ###

<pre><code>
expand_mask(Mask::binary()) -&gt; binary()
</code></pre>
<br />

<a name="hex2bin-1"></a>

### hex2bin/1 ###

<pre><code>
hex2bin(X1::binary()) -&gt; binary()
</code></pre>
<br />

transform a hexadecimal string in (little-endian)

<a name="ibin2hex-1"></a>

### ibin2hex/1 ###

<pre><code>
ibin2hex(Bin::binary()) -&gt; binary()
</code></pre>
<br />

transform a binary string in its hexadecimal representation.

<a name="ihex2bin-1"></a>

### ihex2bin/1 ###

<pre><code>
ihex2bin(X1::binary()) -&gt; binary()
</code></pre>
<br />

transform a hexadecimal string in (big-endian)

<a name="join-2"></a>

### join/2 ###

<pre><code>
join(Tail::[binary()], Sep::binary()) -&gt; binary()
</code></pre>
<br />

<a name="lpad-3"></a>

### lpad/3 ###

<pre><code>
lpad(Input::binary(), PadLen::integer(), PadStr::binary() | undefined) -&gt; binary()
</code></pre>
<br />

<a name="ltrim-2"></a>

### ltrim/2 ###

<pre><code>
ltrim(Text::binary(), Chars::[byte()]) -&gt; binary()
</code></pre>
<br />

<a name="pad-3"></a>

### pad/3 ###

<pre><code>
pad(Bin::binary(), Size::integer(), PadStr::binary() | undefined) -&gt; binary()
</code></pre>
<br />

<a name="repeat-2"></a>

### repeat/2 ###

<pre><code>
repeat(Num::pos_integer(), Byte::byte() | binary()) -&gt; binary()
</code></pre>
<br />

repeat the byte passed as param as many times as the number passed as param.

<a name="reverse-1"></a>

### reverse/1 ###

<pre><code>
reverse(Str::binary()) -&gt; binary()
</code></pre>
<br />

get the reverse of a string passed as param.
Reference: [`https://stackoverflow.com/a/43310493`](https://stackoverflow.com/a/43310493)

<a name="rpad-3"></a>

### rpad/3 ###

<pre><code>
rpad(Input::binary(), PadLen::integer(), PadStr::binary() | undefined) -&gt; binary()
</code></pre>
<br />

<a name="rtrim-2"></a>

### rtrim/2 ###

<pre><code>
rtrim(Text::binary(), Chars::[byte()]) -&gt; binary()
</code></pre>
<br />

<a name="spaces-1"></a>

### spaces/1 ###

<pre><code>
spaces(Num::pos_integer()) -&gt; binary()
</code></pre>
<br />

generate as many spaces as the number passed as param.

<a name="to_lower-1"></a>

### to_lower/1 ###

<pre><code>
to_lower(Text::integer() | binary() | undefined) -&gt; binary() | undefined
</code></pre>
<br />

<a name="to_upper-1"></a>

### to_upper/1 ###

<pre><code>
to_upper(Text::integer() | binary()) -&gt; binary()
</code></pre>
<br />

<a name="trim-1"></a>

### trim/1 ###

<pre><code>
trim(Text::binary()) -&gt; binary()
</code></pre>
<br />

<a name="trim-2"></a>

### trim/2 ###

<pre><code>
trim(Text::binary(), Chars::[byte()]) -&gt; binary()
</code></pre>
<br />

<a name="vsn_cmp-2"></a>

### vsn_cmp/2 ###

<pre><code>
vsn_cmp(Vsn1::binary(), Vsn2::binary()) -&gt; integer()
</code></pre>
<br />

