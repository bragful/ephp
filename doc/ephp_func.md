

# Module ephp_func #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-config_param">config_param()</a> ###


<pre><code>
config_param() = <a href="#type-mixed">mixed()</a>
</code></pre>




### <a name="type-config_section">config_section()</a> ###


<pre><code>
config_section() = binary()
</code></pre>




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
php_config_results() = [{<a href="#type-config_section">config_section()</a>, [<a href="#type-config_param">config_param()</a>]}]
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
php_function_opt() = pack_args | {namespace, <a href="#type-namespace">namespace()</a>} | {alias, <a href="#type-php_function_alias">php_function_alias()</a>} | <a href="#type-php_function_args">php_function_args()</a>
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#destroy-1">destroy/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_functions-1">get_functions/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_static_value-3">get_static_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_static_value-4">get_static_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#init_static_value-4">init_static_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#init_static_value-5">init_static_value/5</a></td><td></td></tr><tr><td valign="top"><a href="#is_defined-2">is_defined/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_defined-3">is_defined/3</a></td><td></td></tr><tr><td valign="top"><a href="#register_func-6">register_func/6</a></td><td></td></tr><tr><td valign="top"><a href="#register_func-7">register_func/7</a></td><td></td></tr><tr><td valign="top"><a href="#register_func-8">register_func/8</a></td><td></td></tr><tr><td valign="top"><a href="#run-2">run/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_static-4">set_static/4</a></td><td></td></tr><tr><td valign="top"><a href="#set_static-5">set_static/5</a></td><td></td></tr><tr><td valign="top"><a href="#set_static_value-4">set_static_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#set_static_value-5">set_static_value/5</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="destroy-1"></a>

### destroy/1 ###

`destroy(Funcs) -> any()`

<a name="get-2"></a>

### get/2 ###

`get(Ref, FuncName) -> any()`

<a name="get-3"></a>

### get/3 ###

`get(Ref, NameSpace, FuncName) -> any()`

<a name="get_functions-1"></a>

### get_functions/1 ###

`get_functions(Ref) -> any()`

<a name="get_static_value-3"></a>

### get_static_value/3 ###

`get_static_value(Ref, FuncName, VarName) -> any()`

<a name="get_static_value-4"></a>

### get_static_value/4 ###

`get_static_value(Ref, NameSpace, FuncName, VarName) -> any()`

<a name="init_static_value-4"></a>

### init_static_value/4 ###

`init_static_value(Ref, FuncName, VarName, Value) -> any()`

<a name="init_static_value-5"></a>

### init_static_value/5 ###

`init_static_value(Ref, NameSpace, FuncName, VarName, Value) -> any()`

<a name="is_defined-2"></a>

### is_defined/2 ###

`is_defined(Ref, FuncName) -> any()`

<a name="is_defined-3"></a>

### is_defined/3 ###

`is_defined(Ref, NameSpace, FuncName) -> any()`

<a name="register_func-6"></a>

### register_func/6 ###

`register_func(Ref, File, NS, PHPFunc, Args, Code) -> any()`

<a name="register_func-7"></a>

### register_func/7 ###

`register_func(Ref, File, NS, PHPFunc, Module, Fun, ValArgs) -> any()`

<a name="register_func-8"></a>

### register_func/8 ###

`register_func(Ref, File, NS, PHPFunc, Module, Fun, PackArgs, ValArgs) -> any()`

<a name="run-2"></a>

### run/2 ###

`run(Context, Call) -> any()`

<a name="set_static-4"></a>

### set_static/4 ###

`set_static(Ref, FuncName, Vars, Context) -> any()`

<a name="set_static-5"></a>

### set_static/5 ###

`set_static(Ref, NameSpace, FuncName, Vars, Context) -> any()`

<a name="set_static_value-4"></a>

### set_static_value/4 ###

`set_static_value(Ref, FuncName, VarName, Value) -> any()`

<a name="set_static_value-5"></a>

### set_static_value/5 ###

`set_static_value(Ref, NameSpace, FuncName, VarName, Value) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

