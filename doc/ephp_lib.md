

# Module ephp_lib #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

<a name="types"></a>

## Data Types ##




### <a name="type-config_param">config_param()</a> ###


<pre><code>
config_param() = <a href="#type-mixed">mixed()</a>
</code></pre>

init_const



### <a name="type-config_section">config_section()</a> ###


<pre><code>
config_section() = binary()
</code></pre>




### <a name="type-const_name">const_name()</a> ###


<pre><code>
const_name() = binary()
</code></pre>

init_func



### <a name="type-default_value">default_value()</a> ###


<pre><code>
default_value() = <a href="#type-mixed">mixed()</a>
</code></pre>




### <a name="type-error_return_value">error_return_value()</a> ###


<pre><code>
error_return_value() = <a href="#type-mixed">mixed()</a>
</code></pre>




### <a name="type-max_args">max_args()</a> ###


<pre><code>
max_args() = non_neg_integer()
</code></pre>




### <a name="type-min_args">min_args()</a> ###


<pre><code>
min_args() = non_neg_integer()
</code></pre>




### <a name="type-php_config_results">php_config_results()</a> ###


<pre><code>
php_config_results() = [{<a href="#type-config_section">config_section()</a>, <a href="#type-config_param">config_param()</a>}]
</code></pre>




### <a name="type-php_const_results">php_const_results()</a> ###


<pre><code>
php_const_results() = [{<a href="#type-const_name">const_name()</a>, number() | binary()}]
</code></pre>




### <a name="type-php_function">php_function()</a> ###


<pre><code>
php_function() = atom()
</code></pre>




### <a name="type-php_function_alias">php_function_alias()</a> ###


<pre><code>
php_function_alias() = binary()
</code></pre>




### <a name="type-php_function_args">php_function_args()</a> ###


<pre><code>
php_function_args() = {args, <a href="#type-validation_args">validation_args()</a>}
</code></pre>




### <a name="type-php_function_defs">php_function_defs()</a> ###


<pre><code>
php_function_defs() = <a href="#type-php_function">php_function()</a> | {<a href="#type-php_function">php_function()</a>, <a href="#type-php_function_opts">php_function_opts()</a>}
</code></pre>




### <a name="type-php_function_opt">php_function_opt()</a> ###


<pre><code>
php_function_opt() = pack_args | {namespace, <a href="ephp_ns.md#type-namespace">ephp_ns:namespace()</a>} | {alias, <a href="#type-php_function_alias">php_function_alias()</a>} | <a href="#type-php_function_args">php_function_args()</a>
</code></pre>




### <a name="type-php_function_opts">php_function_opts()</a> ###


<pre><code>
php_function_opts() = [<a href="#type-php_function_opt">php_function_opt()</a>]
</code></pre>




### <a name="type-php_function_results">php_function_results()</a> ###


<pre><code>
php_function_results() = [<a href="#type-php_function_defs">php_function_defs()</a>]
</code></pre>




### <a name="type-type_arg">type_arg()</a> ###


<pre><code>
type_arg() = mixed | string | integer | array | object | resource | raw | type_ref
</code></pre>




### <a name="type-validation_arg">validation_arg()</a> ###


<pre><code>
validation_arg() = {<a href="#type-type_arg">type_arg()</a>, <a href="#type-default_value">default_value()</a>} | <a href="#type-type_arg">type_arg()</a>
</code></pre>




### <a name="type-validation_args">validation_args()</a> ###


<pre><code>
validation_args() = {<a href="#type-min_args">min_args()</a>, <a href="#type-max_args">max_args()</a>, <a href="#type-error_return_value">error_return_value()</a>, [<a href="#type-validation_arg">validation_arg()</a>]} | [<a href="#type-validation_arg">validation_arg()</a>] | undefined | no_resolve
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#register-2">register/2</a></td><td>register a module.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="register-2"></a>

### register/2 ###

<pre><code>
register(Ctx::<a href="ephp.md#type-context_id">ephp:context_id()</a>, Module::module()) -&gt; ok
</code></pre>
<br />

register a module.

__See also:__ [ephp_func](ephp_func.md).

