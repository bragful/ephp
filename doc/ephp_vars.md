

# Module ephp_vars #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#clone-1">clone/1</a></td><td></td></tr><tr><td valign="top"><a href="#del-3">del/3</a></td><td></td></tr><tr><td valign="top"><a href="#destroy-2">destroy/2</a></td><td></td></tr><tr><td valign="top"><a href="#destroy_data-2">destroy_data/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td></td></tr><tr><td valign="top"><a href="#isset-3">isset/3</a></td><td></td></tr><tr><td valign="top"><a href="#ref-5">ref/5</a></td><td></td></tr><tr><td valign="top"><a href="#set-4">set/4</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#zip_args-7">zip_args/7</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="clone-1"></a>

### clone/1 ###

`clone(Vars) -> any()`

<a name="del-3"></a>

### del/3 ###

`del(Vars, VarPath, Context) -> any()`

<a name="destroy-2"></a>

### destroy/2 ###

<pre><code>
destroy(Ctx::<a href="#type-context">context()</a>, VarsRef::<a href="ephp.md#type-vars_id">ephp:vars_id()</a>) -&gt; ok
</code></pre>
<br />

<a name="destroy_data-2"></a>

### destroy_data/2 ###

`destroy_data(Context, ObjRef) -> any()`

<a name="get-2"></a>

### get/2 ###

`get(Vars, VarPath) -> any()`

<a name="get-3"></a>

### get/3 ###

`get(Vars, VarPath, Context) -> any()`

<a name="isset-3"></a>

### isset/3 ###

`isset(Vars, VarPath, Context) -> any()`

<a name="ref-5"></a>

### ref/5 ###

`ref(Vars, VarPath, VarsPID, RefVarPath, Context) -> any()`

<a name="set-4"></a>

### set/4 ###

`set(Vars, VarPath, Value, Context) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="zip_args-7"></a>

### zip_args/7 ###

`zip_args(VarsSrc, VarsDst, ValArgs, FuncArgs, FunctName, Line, Context) -> any()`

