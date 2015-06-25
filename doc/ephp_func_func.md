

# Module ephp_func_func #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_func`](ephp_func.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#func_num_args-2">func_num_args/2</a></td><td></td></tr><tr><td valign="top"><a href="#function_exists-3">function_exists/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_defined_functions-2">get_defined_functions/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#register_shutdown_function-3">register_shutdown_function/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="func_num_args-2"></a>

### func_num_args/2 ###


<pre><code>
func_num_args(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; non_neg_integer()
</code></pre>
<br />


<a name="function_exists-3"></a>

### function_exists/3 ###


<pre><code>
function_exists(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, FuncName::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="get_defined_functions-2"></a>

### get_defined_functions/2 ###


<pre><code>
get_defined_functions(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; '?DICT_TYPE'
</code></pre>
<br />


<a name="init-0"></a>

### init/0 ###


<pre><code>
init() -&gt; [<a href="ephp_func.md#type-php_function">ephp_func:php_function()</a> | {<a href="ephp_func.md#type-php_function">ephp_func:php_function()</a>, <a href="ephp_func.md#type-php_function_alias">ephp_func:php_function_alias()</a>}]
</code></pre>
<br />


<a name="register_shutdown_function-3"></a>

### register_shutdown_function/3 ###


<pre><code>
register_shutdown_function(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, RawArgs::[<a href="#type-var_value">var_value()</a>]) -&gt; ok
</code></pre>
<br />


