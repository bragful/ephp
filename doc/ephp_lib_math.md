

# Module ephp_lib_math #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_lib`](ephp_lib.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#base_convert-5">base_convert/5</a></td><td></td></tr><tr><td valign="top"><a href="#bindec-3">bindec/3</a></td><td></td></tr><tr><td valign="top"><a href="#init_config-0">init_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_const-0">init_const/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#php_abs-3">php_abs/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_acos-3">php_acos/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_acosh-3">php_acosh/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_asin-3">php_asin/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_asinh-3">php_asinh/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_atan-3">php_atan/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_atan2-4">php_atan2/4</a></td><td></td></tr><tr><td valign="top"><a href="#php_atanh-3">php_atanh/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_ceil-3">php_ceil/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_cos-3">php_cos/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_cosh-3">php_cosh/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_exp-3">php_exp/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_floor-3">php_floor/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_log-4">php_log/4</a></td><td></td></tr><tr><td valign="top"><a href="#php_max-4">php_max/4</a></td><td></td></tr><tr><td valign="top"><a href="#php_min-4">php_min/4</a></td><td></td></tr><tr><td valign="top"><a href="#php_pow-4">php_pow/4</a></td><td></td></tr><tr><td valign="top"><a href="#php_round-3">php_round/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_sin-3">php_sin/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_sinh-3">php_sinh/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_sqrt-3">php_sqrt/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_tan-3">php_tan/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_tanh-3">php_tanh/3</a></td><td></td></tr><tr><td valign="top"><a href="#pi-2">pi/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="base_convert-5"></a>

### base_convert/5 ###

<pre><code>
base_convert(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, From::<a href="#type-var_value">var_value()</a>, To::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="bindec-3"></a>

### bindec/3 ###

<pre><code>
bindec(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="init_config-0"></a>

### init_config/0 ###

<pre><code>
init_config() -&gt; <a href="ephp_lib.md#type-php_config_results">ephp_lib:php_config_results()</a>
</code></pre>
<br />

<a name="init_const-0"></a>

### init_const/0 ###

<pre><code>
init_const() -&gt; <a href="ephp_lib.md#type-php_const_results">ephp_lib:php_const_results()</a>
</code></pre>
<br />

<a name="init_func-0"></a>

### init_func/0 ###

<pre><code>
init_func() -&gt; <a href="ephp_lib.md#type-php_function_results">ephp_lib:php_function_results()</a>
</code></pre>
<br />

<a name="php_abs-3"></a>

### php_abs/3 ###

<pre><code>
php_abs(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, NotANumber::number()) -&gt; number()
</code></pre>
<br />

<a name="php_acos-3"></a>

### php_acos/3 ###

<pre><code>
php_acos(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}) -&gt; <a href="#type-php_float">php_float()</a>
</code></pre>
<br />

<a name="php_acosh-3"></a>

### php_acosh/3 ###

<pre><code>
php_acosh(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}) -&gt; float()
</code></pre>
<br />

<a name="php_asin-3"></a>

### php_asin/3 ###

<pre><code>
php_asin(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}) -&gt; float()
</code></pre>
<br />

<a name="php_asinh-3"></a>

### php_asinh/3 ###

<pre><code>
php_asinh(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}) -&gt; float()
</code></pre>
<br />

<a name="php_atan-3"></a>

### php_atan/3 ###

<pre><code>
php_atan(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}) -&gt; float()
</code></pre>
<br />

<a name="php_atan2-4"></a>

### php_atan2/4 ###

<pre><code>
php_atan2(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}, X4::{any(), number()}) -&gt; float()
</code></pre>
<br />

<a name="php_atanh-3"></a>

### php_atanh/3 ###

<pre><code>
php_atanh(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}) -&gt; float()
</code></pre>
<br />

<a name="php_ceil-3"></a>

### php_ceil/3 ###

<pre><code>
php_ceil(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="php_cos-3"></a>

### php_cos/3 ###

<pre><code>
php_cos(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}) -&gt; float()
</code></pre>
<br />

<a name="php_cosh-3"></a>

### php_cosh/3 ###

<pre><code>
php_cosh(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}) -&gt; float()
</code></pre>
<br />

<a name="php_exp-3"></a>

### php_exp/3 ###

<pre><code>
php_exp(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Arg::<a href="#type-var_value">var_value()</a>) -&gt; float()
</code></pre>
<br />

<a name="php_floor-3"></a>

### php_floor/3 ###

<pre><code>
php_floor(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="php_log-4"></a>

### php_log/4 ###

<pre><code>
php_log(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; float()
</code></pre>
<br />

<a name="php_max-4"></a>

### php_max/4 ###

<pre><code>
php_max(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Num1::<a href="#type-var_value">var_value()</a>, Num2::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="php_min-4"></a>

### php_min/4 ###

<pre><code>
php_min(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, Num1::<a href="#type-var_value">var_value()</a>, Num2::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="php_pow-4"></a>

### php_pow/4 ###

<pre><code>
php_pow(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; float() | integer()
</code></pre>
<br />

<a name="php_round-3"></a>

### php_round/3 ###

<pre><code>
php_round(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="php_sin-3"></a>

### php_sin/3 ###

<pre><code>
php_sin(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}) -&gt; float()
</code></pre>
<br />

<a name="php_sinh-3"></a>

### php_sinh/3 ###

<pre><code>
php_sinh(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}) -&gt; float()
</code></pre>
<br />

<a name="php_sqrt-3"></a>

### php_sqrt/3 ###

<pre><code>
php_sqrt(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; float()
</code></pre>
<br />

<a name="php_tan-3"></a>

### php_tan/3 ###

<pre><code>
php_tan(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}) -&gt; float()
</code></pre>
<br />

<a name="php_tanh-3"></a>

### php_tanh/3 ###

<pre><code>
php_tanh(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>, X3::{any(), number()}) -&gt; float()
</code></pre>
<br />

<a name="pi-2"></a>

### pi/2 ###

<pre><code>
pi(Context::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Line::<a href="#type-line">line()</a>) -&gt; float()
</code></pre>
<br />

