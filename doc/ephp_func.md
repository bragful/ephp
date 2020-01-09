

# Module ephp_func #
* [Function Index](#index)
* [Function Details](#functions)

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

