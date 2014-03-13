

# Module ephp_output #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#destroy-1">destroy/1</a></td><td></td></tr><tr><td valign="top"><a href="#flush-1">flush/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#pop-1">pop/1</a></td><td></td></tr><tr><td valign="top"><a href="#push-2">push/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_flush-2">set_flush/2</a></td><td></td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`


<a name="destroy-1"></a>

### destroy/1 ###

`destroy(Output) -> any()`


<a name="flush-1"></a>

### flush/1 ###

`flush(Output) -> any()`


<a name="get-1"></a>

### get/1 ###

`get(Output) -> any()`


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


<a name="pop-1"></a>

### pop/1 ###

`pop(Output) -> any()`


<a name="push-2"></a>

### push/2 ###

`push(Output, Text) -> any()`


<a name="set_flush-2"></a>

### set_flush/2 ###

`set_flush(Output, Flush) -> any()`


<a name="size-1"></a>

### size/1 ###

`size(Output) -> any()`


<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`


<a name="start_link-1"></a>

### start_link/1 ###

`start_link(Flush) -> any()`


<a name="start_link-2"></a>

### start_link/2 ###

`start_link(Flush, FlushHandler) -> any()`


<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


