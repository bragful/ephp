

# Module ephp_lib_info #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#extension_loaded-3">extension_loaded/3</a></td><td></td></tr><tr><td valign="top"><a href="#ini_get-3">ini_get/3</a></td><td></td></tr><tr><td valign="top"><a href="#ini_set-4">ini_set/4</a></td><td></td></tr><tr><td valign="top"><a href="#init_config-0">init_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_const-0">init_const/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#memory_get_peak_usage-3">memory_get_peak_usage/3</a></td><td></td></tr><tr><td valign="top"><a href="#memory_get_usage-3">memory_get_usage/3</a></td><td></td></tr><tr><td valign="top"><a href="#php_logo_guid-2">php_logo_guid/2</a></td><td></td></tr><tr><td valign="top"><a href="#php_sapi_name-2">php_sapi_name/2</a></td><td></td></tr><tr><td valign="top"><a href="#phpinfo-3">phpinfo/3</a></td><td></td></tr><tr><td valign="top"><a href="#phpversion-2">phpversion/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_include_path-3">set_include_path/3</a></td><td></td></tr><tr><td valign="top"><a href="#version_compare-5">version_compare/5</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="extension_loaded-3"></a>

### extension_loaded/3 ###

<pre><code>
extension_loaded(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Name::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="ini_get-3"></a>

### ini_get/3 ###

<pre><code>
ini_get(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-mixed">mixed()</a>
</code></pre>
<br />

<a name="ini_set-4"></a>

### ini_set/4 ###

<pre><code>
ini_set(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="init_config-0"></a>

### init_config/0 ###

<pre><code>
init_config() -&gt; <a href="ephp_func.md#type-php_config_results">ephp_func:php_config_results()</a>
</code></pre>
<br />

<a name="init_const-0"></a>

### init_const/0 ###

<pre><code>
init_const() -&gt; <a href="ephp_func.md#type-php_const_results">ephp_func:php_const_results()</a>
</code></pre>
<br />

<a name="init_func-0"></a>

### init_func/0 ###

<pre><code>
init_func() -&gt; <a href="ephp_func.md#type-php_function_results">ephp_func:php_function_results()</a>
</code></pre>
<br />

<a name="memory_get_peak_usage-3"></a>

### memory_get_peak_usage/3 ###

<pre><code>
memory_get_peak_usage(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, RealUsage::<a href="#type-var_value">var_value()</a>) -&gt; pos_integer()
</code></pre>
<br />

<a name="memory_get_usage-3"></a>

### memory_get_usage/3 ###

<pre><code>
memory_get_usage(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, RealUsage::<a href="#type-var_value">var_value()</a>) -&gt; pos_integer()
</code></pre>
<br />

<a name="php_logo_guid-2"></a>

### php_logo_guid/2 ###

<pre><code>
php_logo_guid(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; binary()
</code></pre>
<br />

<a name="php_sapi_name-2"></a>

### php_sapi_name/2 ###

<pre><code>
php_sapi_name(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; binary()
</code></pre>
<br />

<a name="phpinfo-3"></a>

### phpinfo/3 ###

<pre><code>
phpinfo(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; undefined
</code></pre>
<br />

<a name="phpversion-2"></a>

### phpversion/2 ###

<pre><code>
phpversion(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; binary()
</code></pre>
<br />

<a name="set_include_path-3"></a>

### set_include_path/3 ###

<pre><code>
set_include_path(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="version_compare-5"></a>

### version_compare/5 ###

<pre><code>
version_compare(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Vsn1::<a href="#type-var_value">var_value()</a>, Vsn2::<a href="#type-var_value">var_value()</a>, Op::<a href="#type-var_value">var_value()</a>) -&gt; boolean() | integer()
</code></pre>
<br />

