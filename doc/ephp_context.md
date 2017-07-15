

# Module ephp_context #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call_method-3">call_method/3</a></td><td></td></tr><tr><td valign="top"><a href="#del-2">del/2</a></td><td></td></tr><tr><td valign="top"><a href="#destroy-1">destroy/1</a></td><td></td></tr><tr><td valign="top"><a href="#destroy_all-1">destroy_all/1</a></td><td></td></tr><tr><td valign="top"><a href="#generate_subcontext-1">generate_subcontext/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_active_file-1">get_active_file/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_const-3">get_const/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_current_class-1">get_current_class/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_current_function-1">get_current_function/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_current_function_arity-1">get_current_function_arity/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_errors_id-1">get_errors_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_function-2">get_function/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_functions-1">get_functions/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_meta-2">get_meta/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_output-1">get_output/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_output_handler-1">get_output_handler/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_shutdown_funcs-1">get_shutdown_funcs/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_state-1">get_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_tz-1">get_tz/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_vars-1">get_vars/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_defined_function-2">is_defined_function/2</a></td><td></td></tr><tr><td valign="top"><a href="#isset-2">isset/2</a></td><td></td></tr><tr><td valign="top"><a href="#load-2">load/2</a></td><td></td></tr><tr><td valign="top"><a href="#load_once-2">load_once/2</a></td><td></td></tr><tr><td valign="top"><a href="#register_class-2">register_class/2</a></td><td></td></tr><tr><td valign="top"><a href="#register_const-3">register_const/3</a></td><td></td></tr><tr><td valign="top"><a href="#register_func-5">register_func/5</a></td><td></td></tr><tr><td valign="top"><a href="#register_func-6">register_func/6</a></td><td></td></tr><tr><td valign="top"><a href="#register_shutdown_func-2">register_shutdown_func/2</a></td><td></td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_active_file-2">set_active_file/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_class_alias-3">set_class_alias/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_errors_id-2">set_errors_id/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_global-2">set_global/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_meta-3">set_meta/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_output-2">set_output/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_output_handler-2">set_output_handler/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_tz-2">set_tz/2</a></td><td></td></tr><tr><td valign="top"><a href="#solve-2">solve/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#unregister_shutdown_func-2">unregister_shutdown_func/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call_method-3"></a>

### call_method/3 ###

`call_method(Context, Instance, Call) -> any()`

<a name="del-2"></a>

### del/2 ###

`del(Context, VarPath) -> any()`

<a name="destroy-1"></a>

### destroy/1 ###

`destroy(Context) -> any()`

<a name="destroy_all-1"></a>

### destroy_all/1 ###

`destroy_all(Context) -> any()`

<a name="generate_subcontext-1"></a>

### generate_subcontext/1 ###

`generate_subcontext(Context) -> any()`

<a name="get-2"></a>

### get/2 ###

`get(Context, VarPath) -> any()`

<a name="get_active_file-1"></a>

### get_active_file/1 ###

`get_active_file(Context) -> any()`

<a name="get_const-3"></a>

### get_const/3 ###

`get_const(Context, Name, Index) -> any()`

<a name="get_current_class-1"></a>

### get_current_class/1 ###

`get_current_class(Context) -> any()`

<a name="get_current_function-1"></a>

### get_current_function/1 ###

`get_current_function(Context) -> any()`

<a name="get_current_function_arity-1"></a>

### get_current_function_arity/1 ###

`get_current_function_arity(Context) -> any()`

<a name="get_errors_id-1"></a>

### get_errors_id/1 ###

`get_errors_id(Context) -> any()`

<a name="get_function-2"></a>

### get_function/2 ###

`get_function(Context, FuncName) -> any()`

<a name="get_functions-1"></a>

### get_functions/1 ###

`get_functions(Context) -> any()`

<a name="get_meta-2"></a>

### get_meta/2 ###

`get_meta(Context, Key) -> any()`

<a name="get_output-1"></a>

### get_output/1 ###

`get_output(Context) -> any()`

<a name="get_output_handler-1"></a>

### get_output_handler/1 ###

`get_output_handler(Context) -> any()`

<a name="get_shutdown_funcs-1"></a>

### get_shutdown_funcs/1 ###

`get_shutdown_funcs(Context) -> any()`

<a name="get_state-1"></a>

### get_state/1 ###

`get_state(Context) -> any()`

<a name="get_tz-1"></a>

### get_tz/1 ###

`get_tz(Context) -> any()`

<a name="get_vars-1"></a>

### get_vars/1 ###

`get_vars(Context) -> any()`

<a name="is_defined_function-2"></a>

### is_defined_function/2 ###

`is_defined_function(Context, FuncName) -> any()`

<a name="isset-2"></a>

### isset/2 ###

`isset(Context, VarPath) -> any()`

<a name="load-2"></a>

### load/2 ###

`load(Context, File) -> any()`

<a name="load_once-2"></a>

### load_once/2 ###

`load_once(Context, File) -> any()`

<a name="register_class-2"></a>

### register_class/2 ###

`register_class(Context, Class) -> any()`

<a name="register_const-3"></a>

### register_const/3 ###

`register_const(Context, Name, Value) -> any()`

<a name="register_func-5"></a>

### register_func/5 ###

`register_func(Context, PHPFunc, Module, Fun, VA) -> any()`

<a name="register_func-6"></a>

### register_func/6 ###

`register_func(Context, PHPFunc, Module, Fun, PackArgs, VA) -> any()`

<a name="register_shutdown_func-2"></a>

### register_shutdown_func/2 ###

`register_shutdown_func(Context, FuncName) -> any()`

<a name="set-3"></a>

### set/3 ###

`set(Context, VarPath, Value) -> any()`

<a name="set_active_file-2"></a>

### set_active_file/2 ###

`set_active_file(Context, Filename) -> any()`

<a name="set_class_alias-3"></a>

### set_class_alias/3 ###

`set_class_alias(Context, ClassName, ClassAlias) -> any()`

<a name="set_errors_id-2"></a>

### set_errors_id/2 ###

`set_errors_id(Context, Errors) -> any()`

<a name="set_global-2"></a>

### set_global/2 ###

`set_global(Context, GlobalContext) -> any()`

<a name="set_meta-3"></a>

### set_meta/3 ###

`set_meta(Context, Key, Value) -> any()`

<a name="set_output-2"></a>

### set_output/2 ###

`set_output(Context, Text) -> any()`

<a name="set_output_handler-2"></a>

### set_output_handler/2 ###

`set_output_handler(Context, Output) -> any()`

<a name="set_tz-2"></a>

### set_tz/2 ###

`set_tz(Context, TZ) -> any()`

<a name="solve-2"></a>

### solve/2 ###

`solve(Context, Expression) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="unregister_shutdown_func-2"></a>

### unregister_shutdown_func/2 ###

`unregister_shutdown_func(Context, FuncName) -> any()`

