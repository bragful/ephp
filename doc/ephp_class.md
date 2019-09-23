

# Module ephp_class #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module stores the classes found in the code and other defined
classes here and defined in the `ephp_class_*` modules.

<a name="description"></a>

## Description ##

This module is responsible to handle that information as well.
Retrieves the methods, constructor, destructor, parent class,
attributes, and static data.

This module has the possibility to handle interfaces. The
interfaces are handled in ephp as special classes but
registering only methods and constants.

Finally this module has functions to create instances and
check if a data is an instance of a specific class, interface
or native data.
<a name="types"></a>

## Data Types ##




### <a name="type-alias_return">alias_return()</a> ###


<pre><code>
alias_return() = ok | {error, enoexist | eredefined}
</code></pre>




### <a name="type-get_return">get_return()</a> ###


<pre><code>
get_return() = {ok, <a href="#type-class">class()</a>} | {error, enoexist}
</code></pre>




### <a name="type-loader">loader()</a> ###


<pre><code>
loader() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_if_no_exists_attrib-2">add_if_no_exists_attrib/2</a></td><td></td></tr><tr><td valign="top"><a href="#class_attr-1">class_attr/1</a></td><td></td></tr><tr><td valign="top"><a href="#class_attr-2">class_attr/2</a></td><td></td></tr><tr><td valign="top"><a href="#class_attr-3">class_attr/3</a></td><td></td></tr><tr><td valign="top"><a href="#class_attr-4">class_attr/4</a></td><td></td></tr><tr><td valign="top"><a href="#destroy-1">destroy/1</a></td><td>destroy the classes handler.</td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td>retrieves a class registered given the class name.</td></tr><tr><td valign="top"><a href="#get-4">get/4</a></td><td>retrieves a class registered given the class name.</td></tr><tr><td valign="top"><a href="#get_attribute-2">get_attribute/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_clone-2">get_clone/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_constructor-2">get_constructor/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_destructor-2">get_destructor/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_method-2">get_method/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_method-3">get_method/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_parent-3">get_parent/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_stdclass-0">get_stdclass/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_static_value-6">init_static_value/6</a></td><td></td></tr><tr><td valign="top"><a href="#instance-6">instance/6</a></td><td></td></tr><tr><td valign="top"><a href="#register_class-4">register_class/4</a></td><td>register a class inside of the classes handler.</td></tr><tr><td valign="top"><a href="#register_classes-2">register_classes/2</a></td><td></td></tr><tr><td valign="top"><a href="#register_classes-3">register_classes/3</a></td><td></td></tr><tr><td valign="top"><a href="#register_interface-3">register_interface/3</a></td><td></td></tr><tr><td valign="top"><a href="#register_loader-2">register_loader/2</a></td><td>register loader for the classes handler.</td></tr><tr><td valign="top"><a href="#set_alias-5">set_alias/5</a></td><td>set a name as alias of a class name.</td></tr><tr><td valign="top"><a href="#set_static-6">set_static/6</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>starts a classes handler.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_if_no_exists_attrib-2"></a>

### add_if_no_exists_attrib/2 ###

`add_if_no_exists_attrib(Class, Names) -> any()`

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

<pre><code>
destroy(Classes::<a href="ephp.md#type-classes_id">ephp:classes_id()</a>) -&gt; ok
</code></pre>
<br />

destroy the classes handler.

<a name="get-3"></a>

### get/3 ###

<pre><code>
get(Ref::<a href="ephp.md#type-classes_id">ephp:classes_id()</a>, NS::<a href="#type-namespace">namespace()</a>, ClassName::<a href="#type-class_name">class_name()</a>) -&gt; <a href="#type-get_return">get_return()</a>
</code></pre>
<br />

retrieves a class registered given the class name.

<a name="get-4"></a>

### get/4 ###

<pre><code>
get(Context::<a href="#type-context">context()</a>, NS::<a href="#type-namespace">namespace()</a>, ClassName::<a href="#type-class_name">class_name()</a>, AutoLoad::boolean() | spl) -&gt; <a href="#type-get_return">get_return()</a>
</code></pre>
<br />

retrieves a class registered given the class name.

<a name="get_attribute-2"></a>

### get_attribute/2 ###

`get_attribute(Class, AttributeName) -> any()`

<a name="get_clone-2"></a>

### get_clone/2 ###

`get_clone(Ref, Class) -> any()`

<a name="get_constructor-2"></a>

### get_constructor/2 ###

`get_constructor(Ref, Class) -> any()`

<a name="get_destructor-2"></a>

### get_destructor/2 ###

`get_destructor(Ref, Class) -> any()`

<a name="get_method-2"></a>

### get_method/2 ###

`get_method(Class, MethodName) -> any()`

<a name="get_method-3"></a>

### get_method/3 ###

`get_method(Class, Index, MethodName) -> any()`

<a name="get_parent-3"></a>

### get_parent/3 ###

`get_parent(Context, NS, Name) -> any()`

<a name="get_stdclass-0"></a>

### get_stdclass/0 ###

`get_stdclass() -> any()`

<a name="init_static_value-6"></a>

### init_static_value/6 ###

`init_static_value(Ref, NS, ClassName, MethodName, VarName, Value) -> any()`

<a name="instance-6"></a>

### instance/6 ###

`instance(Ref, LocalCtx, GlobalCtx, ClassNS, ClassName, Line) -> any()`

<a name="register_class-4"></a>

### register_class/4 ###

<pre><code>
register_class(Ref::<a href="ephp.md#type-classes_id">ephp:classes_id()</a>, File::binary(), GlobalCtx::<a href="#type-context">context()</a>, Class::<a href="#type-class">class()</a>) -&gt; ok
</code></pre>
<br />

register a class inside of the classes handler.

<a name="register_classes-2"></a>

### register_classes/2 ###

`register_classes(ClassesRef, Context) -> any()`

<a name="register_classes-3"></a>

### register_classes/3 ###

`register_classes(ClassesRef, Context, Classes) -> any()`

<a name="register_interface-3"></a>

### register_interface/3 ###

`register_interface(Ref, File, Class) -> any()`

<a name="register_loader-2"></a>

### register_loader/2 ###

<pre><code>
register_loader(Ref::<a href="ephp.md#type-classes_id">ephp:classes_id()</a>, Loader::<a href="#type-loader">loader()</a>) -&gt; ok
</code></pre>
<br />

register loader for the classes handler.

<a name="set_alias-5"></a>

### set_alias/5 ###

<pre><code>
set_alias(Ref::<a href="ephp.md#type-classes_id">ephp:classes_id()</a>, NS::<a href="#type-namespace">namespace()</a>, ClassName::<a href="#type-class_name">class_name()</a>, NSAlias::<a href="#type-namespace">namespace()</a>, Alias::<a href="#type-class_name">class_name()</a>) -&gt; <a href="#type-alias_return">alias_return()</a>
</code></pre>
<br />

set a name as alias of a class name.

<a name="set_static-6"></a>

### set_static/6 ###

`set_static(Ref, NS, ClassName, MethodName, Vars, Context) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, <a href="ephp.md#type-classes_id">ephp:classes_id()</a>}
</code></pre>
<br />

starts a classes handler.

