

# Module ephp_lib_string #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_lib`](ephp_lib.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bin2hex-3">bin2hex/3</a></td><td></td></tr><tr><td valign="top"><a href="#chr-3">chr/3</a></td><td>obtain the character giving the number as a param.</td></tr><tr><td valign="top"><a href="#count_chars-4">count_chars/4</a></td><td></td></tr><tr><td valign="top"><a href="#explode-4">explode/4</a></td><td>split the string in pieces in an array.</td></tr><tr><td valign="top"><a href="#explode-5">explode/5</a></td><td>split the string in pieces in an array with a limit.</td></tr><tr><td valign="top"><a href="#handle_error-3">handle_error/3</a></td><td>handle error messages.</td></tr><tr><td valign="top"><a href="#hex2bin-3">hex2bin/3</a></td><td></td></tr><tr><td valign="top"><a href="#implode-3">implode/3</a></td><td>join the array passed as a param.</td></tr><tr><td valign="top"><a href="#implode-4">implode/4</a></td><td>join the array with the glue passed as a param.</td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#lcfirst-3">lcfirst/3</a></td><td></td></tr><tr><td valign="top"><a href="#ltrim-4">ltrim/4</a></td><td></td></tr><tr><td valign="top"><a href="#md5-4">md5/4</a></td><td></td></tr><tr><td valign="top"><a href="#nl2br-4">nl2br/4</a></td><td></td></tr><tr><td valign="top"><a href="#ord-3">ord/3</a></td><td>obtain the number of the character passed as a param.</td></tr><tr><td valign="top"><a href="#print-3">print/3</a></td><td></td></tr><tr><td valign="top"><a href="#printf-3">printf/3</a></td><td>print using a format.</td></tr><tr><td valign="top"><a href="#rtrim-4">rtrim/4</a></td><td></td></tr><tr><td valign="top"><a href="#sprintf-3">sprintf/3</a></td><td>generate a string using a format.</td></tr><tr><td valign="top"><a href="#str_pad-6">str_pad/6</a></td><td></td></tr><tr><td valign="top"><a href="#str_repeat-4">str_repeat/4</a></td><td></td></tr><tr><td valign="top"><a href="#str_replace-5">str_replace/5</a></td><td></td></tr><tr><td valign="top"><a href="#str_replace-6">str_replace/6</a></td><td></td></tr><tr><td valign="top"><a href="#str_shuffle-3">str_shuffle/3</a></td><td></td></tr><tr><td valign="top"><a href="#str_split-3">str_split/3</a></td><td></td></tr><tr><td valign="top"><a href="#str_split-4">str_split/4</a></td><td></td></tr><tr><td valign="top"><a href="#strlen-3">strlen/3</a></td><td>retrieve the lenght of the string.</td></tr><tr><td valign="top"><a href="#strpos-5">strpos/5</a></td><td></td></tr><tr><td valign="top"><a href="#strrev-3">strrev/3</a></td><td></td></tr><tr><td valign="top"><a href="#strrpos-5">strrpos/5</a></td><td></td></tr><tr><td valign="top"><a href="#strtolower-3">strtolower/3</a></td><td></td></tr><tr><td valign="top"><a href="#strtoupper-3">strtoupper/3</a></td><td></td></tr><tr><td valign="top"><a href="#substr-5">substr/5</a></td><td></td></tr><tr><td valign="top"><a href="#trim-4">trim/4</a></td><td></td></tr><tr><td valign="top"><a href="#ucfirst-3">ucfirst/3</a></td><td></td></tr><tr><td valign="top"><a href="#ucwords-4">ucwords/4</a></td><td></td></tr><tr><td valign="top"><a href="#vprintf-4">vprintf/4</a></td><td></td></tr><tr><td valign="top"><a href="#vsprintf-4">vsprintf/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bin2hex-3"></a>

### bin2hex/3 ###

<pre><code>
bin2hex(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="chr-3"></a>

### chr/3 ###

<pre><code>
chr(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Integer::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

obtain the character giving the number as a param.

<a name="count_chars-4"></a>

### count_chars/4 ###

<pre><code>
count_chars(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; <a href="ephp_array.md#type-ephp_array">ephp_array:ephp_array()</a> | binary()
</code></pre>
<br />

<a name="explode-4"></a>

### explode/4 ###

<pre><code>
explode(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Delimiter::<a href="#type-var_value">var_value()</a>, String::<a href="#type-var_value">var_value()</a>) -&gt; <a href="ephp_array.md#type-ephp_array">ephp_array:ephp_array()</a>
</code></pre>
<br />

split the string in pieces in an array.

<a name="explode-5"></a>

### explode/5 ###

<pre><code>
explode(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Delimiter::<a href="#type-var_value">var_value()</a>, String::<a href="#type-var_value">var_value()</a>, Limit::<a href="#type-var_value">var_value()</a>) -&gt; <a href="ephp_array.md#type-ephp_array">ephp_array:ephp_array()</a>
</code></pre>
<br />

split the string in pieces in an array with a limit.

<a name="handle_error-3"></a>

### handle_error/3 ###

<pre><code>
handle_error(Type::<a href="ephp_error.md#type-error_type">ephp_error:error_type()</a>, Level::<a href="ephp_error.md#type-error_level">ephp_error:error_level()</a>, Args::term()) -&gt; string() | ignore
</code></pre>
<br />

handle error messages.

<a name="hex2bin-3"></a>

### hex2bin/3 ###

<pre><code>
hex2bin(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="implode-3"></a>

### implode/3 ###

<pre><code>
implode(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Pieces::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

join the array passed as a param.

<a name="implode-4"></a>

### implode/4 ###

<pre><code>
implode(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Glue::<a href="#type-var_value">var_value()</a>, Pieces::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

join the array with the glue passed as a param.

<a name="init_func-0"></a>

### init_func/0 ###

<pre><code>
init_func() -&gt; <a href="ephp_lib.md#type-php_function_results">ephp_lib:php_function_results()</a>
</code></pre>
<br />

<a name="lcfirst-3"></a>

### lcfirst/3 ###

<pre><code>
lcfirst(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Text::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="ltrim-4"></a>

### ltrim/4 ###

<pre><code>
ltrim(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Str::<a href="#type-var_value">var_value()</a>, CharMask::<a href="#type-var_value">var_value()</a>) -&gt; binary() | undefined
</code></pre>
<br />

<a name="md5-4"></a>

### md5/4 ###

<pre><code>
md5(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="nl2br-4"></a>

### nl2br/4 ###

<pre><code>
nl2br(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="ord-3"></a>

### ord/3 ###

<pre><code>
ord(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, String::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

obtain the number of the character passed as a param.

<a name="print-3"></a>

### print/3 ###

<pre><code>
print(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Values::[<a href="#type-var_value">var_value()</a>]) -&gt; 1
</code></pre>
<br />

<a name="printf-3"></a>

### printf/3 ###

<pre><code>
printf(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Values::[<a href="#type-var_value">var_value()</a>]) -&gt; pos_integer()
</code></pre>
<br />

print using a format.

<a name="rtrim-4"></a>

### rtrim/4 ###

<pre><code>
rtrim(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Str::<a href="#type-var_value">var_value()</a>, CharMask::<a href="#type-var_value">var_value()</a>) -&gt; binary() | undefined
</code></pre>
<br />

<a name="sprintf-3"></a>

### sprintf/3 ###

<pre><code>
sprintf(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Values::[<a href="#type-var_value">var_value()</a>]) -&gt; binary()
</code></pre>
<br />

generate a string using a format.

<a name="str_pad-6"></a>

### str_pad/6 ###

<pre><code>
str_pad(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>, X5::<a href="#type-var_value">var_value()</a>, X6::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="str_repeat-4"></a>

### str_repeat/4 ###

<pre><code>
str_repeat(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; binary() | undefined
</code></pre>
<br />

<a name="str_replace-5"></a>

### str_replace/5 ###

<pre><code>
str_replace(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Search::<a href="#type-var_value">var_value()</a>, Replace::<a href="#type-var_value">var_value()</a>, Subject::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="str_replace-6"></a>

### str_replace/6 ###

<pre><code>
str_replace(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Search::<a href="#type-var_value">var_value()</a>, Replace::<a href="#type-var_value">var_value()</a>, Subject::<a href="#type-var_value">var_value()</a>, Count::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="str_shuffle-3"></a>

### str_shuffle/3 ###

<pre><code>
str_shuffle(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="str_split-3"></a>

### str_split/3 ###

<pre><code>
str_split(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Text::<a href="#type-var_value">var_value()</a>) -&gt; <a href="ephp_array.md#type-ephp_array">ephp_array:ephp_array()</a>
</code></pre>
<br />

<a name="str_split-4"></a>

### str_split/4 ###

<pre><code>
str_split(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Text::<a href="#type-var_value">var_value()</a>, Size::<a href="#type-var_value">var_value()</a>) -&gt; <a href="ephp_array.md#type-ephp_array">ephp_array:ephp_array()</a> | undefined
</code></pre>
<br />

<a name="strlen-3"></a>

### strlen/3 ###

<pre><code>
strlen(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, String::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

retrieve the lenght of the string.

<a name="strpos-5"></a>

### strpos/5 ###

<pre><code>
strpos(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, HayStack::<a href="#type-var_value">var_value()</a>, Needle::<a href="#type-var_value">var_value()</a>, Offset::<a href="#type-var_value">var_value()</a>) -&gt; false | pos_integer() | undefined
</code></pre>
<br />

<a name="strrev-3"></a>

### strrev/3 ###

<pre><code>
strrev(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Str::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="strrpos-5"></a>

### strrpos/5 ###

<pre><code>
strrpos(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, HayStack::<a href="#type-var_value">var_value()</a>, Needle::<a href="#type-var_value">var_value()</a>, Offset::<a href="#type-var_value">var_value()</a>) -&gt; false | pos_integer() | undefined
</code></pre>
<br />

<a name="strtolower-3"></a>

### strtolower/3 ###

<pre><code>
strtolower(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Text::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="strtoupper-3"></a>

### strtoupper/3 ###

<pre><code>
strtoupper(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Text::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="substr-5"></a>

### substr/5 ###

<pre><code>
substr(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>, X5::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="trim-4"></a>

### trim/4 ###

<pre><code>
trim(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Str::<a href="#type-var_value">var_value()</a>, CharMask::<a href="#type-var_value">var_value()</a>) -&gt; binary() | undefined
</code></pre>
<br />

<a name="ucfirst-3"></a>

### ucfirst/3 ###

<pre><code>
ucfirst(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Text::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="ucwords-4"></a>

### ucwords/4 ###

<pre><code>
ucwords(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="vprintf-4"></a>

### vprintf/4 ###

<pre><code>
vprintf(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Format::<a href="#type-var_value">var_value()</a>, Values::<a href="#type-var_value">var_value()</a>) -&gt; pos_integer()
</code></pre>
<br />

<a name="vsprintf-4"></a>

### vsprintf/4 ###

<pre><code>
vsprintf(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Format::<a href="#type-var_value">var_value()</a>, Values::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

