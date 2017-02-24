

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




### <a name="type-php_config_results">php_config_results()</a> ###


<pre><code>
php_config_results() = [{<a href="#type-config_section">config_section()</a>, [<a href="#type-config_param">config_param()</a>]}]
</code></pre>




### <a name="type-php_function">php_function()</a> ###


<pre><code>
php_function() = atom()
</code></pre>




### <a name="type-php_function_opt">php_function_opt()</a> ###


<pre><code>
php_function_opt() = atom() | {atom(), any()}
</code></pre>




### <a name="type-php_function_opts">php_function_opts()</a> ###


<pre><code>
php_function_opts() = [<a href="#type-php_function_opts">php_function_opts()</a>]
</code></pre>




### <a name="type-php_function_result">php_function_result()</a> ###


<pre><code>
php_function_result() = <a href="#type-php_function">php_function()</a> | {<a href="#type-php_function">php_function()</a>, php_function_opts}
</code></pre>




### <a name="type-php_function_results">php_function_results()</a> ###


<pre><code>
php_function_results() = [<a href="#type-php_function_result">php_function_result()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#destroy-1">destroy/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_functions-1">get_functions/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_func-3">register_func/3</a></td><td></td></tr><tr><td valign="top"><a href="#register_func-4">register_func/4</a></td><td></td></tr><tr><td valign="top"><a href="#register_func-5">register_func/5</a></td><td></td></tr><tr><td valign="top"><a href="#run-2">run/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="destroy-1"></a>

### destroy/1 ###

`destroy(Funcs) -> any()`

<a name="get-2"></a>

### get/2 ###

`get(Ref, FuncName) -> any()`

<a name="get_functions-1"></a>

### get_functions/1 ###

`get_functions(Ref) -> any()`

<a name="register_func-3"></a>

### register_func/3 ###

`register_func(Ref, PHPFunc, Fun) -> any()`

<a name="register_func-4"></a>

### register_func/4 ###

`register_func(Ref, PHPFunc, Module, Fun) -> any()`

<a name="register_func-5"></a>

### register_func/5 ###

`register_func(Ref, PHPFunc, Module, Fun, PackArgs) -> any()`

<a name="run-2"></a>

### run/2 ###

`run(Context, Call) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

