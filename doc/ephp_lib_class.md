

# Module ephp_lib_class #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_func`](ephp_func.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#class_alias-4">class_alias/4</a></td><td></td></tr><tr><td valign="top"><a href="#class_exists-4">class_exists/4</a></td><td></td></tr><tr><td valign="top"><a href="#get_class-3">get_class/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_error-3">handle_error/3</a></td><td></td></tr><tr><td valign="top"><a href="#init_config-0">init_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_const-0">init_const/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#interface_exists-4">interface_exists/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="class_alias-4"></a>

### class_alias/4 ###

<pre><code>
class_alias(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, ClassName::<a href="#type-var_value">var_value()</a>, ClassAlias::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="class_exists-4"></a>

### class_exists/4 ###

<pre><code>
class_exists(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Class::<a href="#type-var_value">var_value()</a>, AutoLoad::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="get_class-3"></a>

### get_class/3 ###

<pre><code>
get_class(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Class::<a href="#type-var_value">var_value()</a>) -&gt; any()
</code></pre>
<br />

<a name="handle_error-3"></a>

### handle_error/3 ###

<pre><code>
handle_error(Type::<a href="ephp_error.md#type-error_type">ephp_error:error_type()</a>, Level::<a href="ephp_error.md#type-error_level">ephp_error:error_level()</a>, Args::term()) -&gt; string() | ignore
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

<a name="interface_exists-4"></a>

### interface_exists/4 ###

<pre><code>
interface_exists(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Class::<a href="#type-var_value">var_value()</a>, AutoLoad::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

