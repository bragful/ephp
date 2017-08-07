

# Module ephp_class #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_if_no_exists_attrib-2">add_if_no_exists_attrib/2</a></td><td></td></tr><tr><td valign="top"><a href="#class_attr-1">class_attr/1</a></td><td></td></tr><tr><td valign="top"><a href="#class_attr-2">class_attr/2</a></td><td></td></tr><tr><td valign="top"><a href="#class_attr-3">class_attr/3</a></td><td></td></tr><tr><td valign="top"><a href="#class_attr-4">class_attr/4</a></td><td></td></tr><tr><td valign="top"><a href="#destroy-1">destroy/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_attribute-2">get_attribute/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_const-2">get_const/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_const-3">get_const/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_constructor-1">get_constructor/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_consts-1">get_consts/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_consts-2">get_consts/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_destructor-1">get_destructor/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_method-3">get_method/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_stdclass-0">get_stdclass/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_static_value-5">init_static_value/5</a></td><td></td></tr><tr><td valign="top"><a href="#instance-5">instance/5</a></td><td></td></tr><tr><td valign="top"><a href="#instance_of-2">instance_of/2</a></td><td></td></tr><tr><td valign="top"><a href="#register_class-4">register_class/4</a></td><td></td></tr><tr><td valign="top"><a href="#register_interface-3">register_interface/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_alias-3">set_alias/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_static-4">set_static/4</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_if_no_exists_attrib-2"></a>

### add_if_no_exists_attrib/2 ###

`add_if_no_exists_attrib(Class, Name) -> any()`

<a name="class_attr-1"></a>

### class_attr/1 ###

`class_attr(Name) -> any()`

<a name="class_attr-2"></a>

### class_attr/2 ###

`class_attr(Name, Access) -> any()`

<a name="class_attr-3"></a>

### class_attr/3 ###

`class_attr(Name, Access, InitValue) -> any()`

<a name="class_attr-4"></a>

### class_attr/4 ###

`class_attr(Name, Access, InitValue, Final) -> any()`

<a name="destroy-1"></a>

### destroy/1 ###

`destroy(Classes) -> any()`

<a name="get-2"></a>

### get/2 ###

`get(Ref, ClassName) -> any()`

<a name="get_attribute-2"></a>

### get_attribute/2 ###

`get_attribute(Class, AttributeName) -> any()`

<a name="get_const-2"></a>

### get_const/2 ###

`get_const(Class, ConstName) -> any()`

<a name="get_const-3"></a>

### get_const/3 ###

`get_const(Ref, ClassName, ConstName) -> any()`

<a name="get_constructor-1"></a>

### get_constructor/1 ###

`get_constructor(Class) -> any()`

<a name="get_consts-1"></a>

### get_consts/1 ###

`get_consts(Class) -> any()`

<a name="get_consts-2"></a>

### get_consts/2 ###

`get_consts(Ref, ClassName) -> any()`

<a name="get_destructor-1"></a>

### get_destructor/1 ###

`get_destructor(Class) -> any()`

<a name="get_method-3"></a>

### get_method/3 ###

`get_method(Class, Index, MethodName) -> any()`

<a name="get_stdclass-0"></a>

### get_stdclass/0 ###

`get_stdclass() -> any()`

<a name="init_static_value-5"></a>

### init_static_value/5 ###

`init_static_value(Ref, ClassName, MethodName, VarName, Value) -> any()`

<a name="instance-5"></a>

### instance/5 ###

`instance(Ref, LocalCtx, GlobalCtx, RawClassName, Line) -> any()`

<a name="instance_of-2"></a>

### instance_of/2 ###

<pre><code>
instance_of(Ephp_array::<a href="#type-mixed">mixed()</a>, DataType::binary()) -&gt; boolean()
</code></pre>
<br />

<a name="register_class-4"></a>

### register_class/4 ###

`register_class(Ref, File, GlobalCtx, Class) -> any()`

<a name="register_interface-3"></a>

### register_interface/3 ###

`register_interface(Ref, File, Class) -> any()`

<a name="set_alias-3"></a>

### set_alias/3 ###

`set_alias(Ref, ClassName, AliasName) -> any()`

<a name="set_static-4"></a>

### set_static/4 ###

`set_static(Ref, ClassName, MethodName, Vars) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

