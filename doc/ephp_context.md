

# Module ephp_context #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call_func-3">call_func/3</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#destroy-1">destroy/1</a></td><td></td></tr><tr><td valign="top"><a href="#generate_subcontext-1">generate_subcontext/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_output-1">get_output/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_state-1">get_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_tz-1">get_tz/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_func-3">register_func/3</a></td><td></td></tr><tr><td valign="top"><a href="#register_func-4">register_func/4</a></td><td></td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_global-2">set_global/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_output-2">set_output/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_tz-2">set_tz/2</a></td><td></td></tr><tr><td valign="top"><a href="#solve-2">solve/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call_func-3"></a>

### call_func/3 ###

`call_func(Context, PHPFunc, Args) -> any()`


<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`


<a name="destroy-1"></a>

### destroy/1 ###

`destroy(Context) -> any()`


<a name="generate_subcontext-1"></a>

### generate_subcontext/1 ###

`generate_subcontext(Context) -> any()`


<a name="get-2"></a>

### get/2 ###

`get(Context, VarPath) -> any()`


<a name="get_output-1"></a>

### get_output/1 ###

`get_output(Context) -> any()`


<a name="get_state-1"></a>

### get_state/1 ###

`get_state(Context) -> any()`


<a name="get_tz-1"></a>

### get_tz/1 ###

`get_tz(Context) -> any()`


<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`


<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`


<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`


<a name="register_func-3"></a>

### register_func/3 ###

`register_func(Context, PHPFunc, Fun) -> any()`


<a name="register_func-4"></a>

### register_func/4 ###

`register_func(Context, PHPFunc, Module, Fun) -> any()`


<a name="set-3"></a>

### set/3 ###

`set(Context, VarPath, Value) -> any()`


<a name="set_global-2"></a>

### set_global/2 ###

`set_global(Context, GlobalContext) -> any()`


<a name="set_output-2"></a>

### set_output/2 ###

`set_output(Context, Text) -> any()`


<a name="set_tz-2"></a>

### set_tz/2 ###

`set_tz(Context, TZ) -> any()`


<a name="solve-2"></a>

### solve/2 ###

`solve(Context, Expression) -> any()`


<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`


<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


