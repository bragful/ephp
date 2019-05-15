

# Module ephp_object #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This module is intended to store the object collection based on an array.

<a name="description"></a>

## Description ##

The original PHP has an array to collect all of the instances from all of the
possible objects. When a new instance is generated, it's allocated in an
empty position of that array.

The variables only referenciate to that object so, all of the variables
which contains objects are pointers indeed.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-2">add/2</a></td><td>stores an object searching an empty slot for it and returning the
Object ID where it was stored.</td></tr><tr><td valign="top"><a href="#add_link-1">add_link/1</a></td><td>Equivalent to <tt>add_link / 2</tt>.</td></tr><tr><td valign="top"><a href="#add_link-2">add_link/2</a></td><td>increases the number of links for an object.</td></tr><tr><td valign="top"><a href="#clone-2">clone/2</a></td><td>clones an object generating a new one and executing its <code>__clone</code>
function by the way.</td></tr><tr><td valign="top"><a href="#destroy-2">destroy/2</a></td><td>Destroy the Objects storage.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>retrieves an object based on the Object ID.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>retrieves an object based on the Object ID.</td></tr><tr><td valign="top"><a href="#get_class-1">get_class/1</a></td><td>retrieves the class record for a provided Object ID.</td></tr><tr><td valign="top"><a href="#get_class-2">get_class/2</a></td><td>retrieves the class record for a provided Object ID.</td></tr><tr><td valign="top"><a href="#get_class_name-1">get_class_name/1</a></td><td>Equivalent to <tt>get_class_name / 2</tt>.</td></tr><tr><td valign="top"><a href="#get_class_name-2">get_class_name/2</a></td><td>retrieves the class name for a provided Object ID.</td></tr><tr><td valign="top"><a href="#get_context-1">get_context/1</a></td><td>Equivalent to <tt>get_context / 2</tt>.</td></tr><tr><td valign="top"><a href="#get_context-2">get_context/2</a></td><td>retrieves the object context for an provided Object ID.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>Equivalent to <tt>remove / 3</tt>.</td></tr><tr><td valign="top"><a href="#remove-3">remove/3</a></td><td>decreases the number of links for an object.</td></tr><tr><td valign="top"><a href="#remove_all-2">remove_all/2</a></td><td>removes all of the objects stored using objects_id.</td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td>stores an object given the Object ID.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>stores an object given the Object ID.</td></tr><tr><td valign="top"><a href="#set_class-2">set_class/2</a></td><td>set a class for an existent object.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>creates a new Objects storage.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-2"></a>

### add/2 ###

<pre><code>
add(Objects::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, Object::<a href="#type-ephp_object">ephp_object()</a>) -&gt; <a href="#type-object_id">object_id()</a>
</code></pre>
<br />

stores an object searching an empty slot for it and returning the
Object ID where it was stored.

<a name="add_link-1"></a>

### add_link/1 ###

<pre><code>
add_link(Obj_ref::<a href="#type-obj_ref">obj_ref()</a>) -&gt; ok
</code></pre>
<br />

Equivalent to `add_link / 2`.

<a name="add_link-2"></a>

### add_link/2 ###

<pre><code>
add_link(Objects::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, ObjectId::<a href="#type-object_id">object_id()</a>) -&gt; ok
</code></pre>
<br />

increases the number of links for an object.

<a name="clone-2"></a>

### clone/2 ###

<pre><code>
clone(Context::<a href="#type-context">context()</a>, Obj_ref::<a href="#type-obj_ref">obj_ref()</a>) -&gt; <a href="#type-obj_ref">obj_ref()</a>
</code></pre>
<br />

clones an object generating a new one and executing its `__clone`
function by the way.

<a name="destroy-2"></a>

### destroy/2 ###

<pre><code>
destroy(Context::<a href="#type-context">context()</a>, Ref::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>) -&gt; ok
</code></pre>
<br />

Destroy the Objects storage.

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(Obj_ref::<a href="#type-obj_ref">obj_ref()</a>) -&gt; undefined | <a href="#type-ephp_object">ephp_object()</a>
</code></pre>
<br />

retrieves an object based on the Object ID.

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Ref::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, ObjectId::<a href="#type-object_id">object_id()</a>) -&gt; undefined | <a href="#type-ephp_object">ephp_object()</a>
</code></pre>
<br />

retrieves an object based on the Object ID.

<a name="get_class-1"></a>

### get_class/1 ###

<pre><code>
get_class(Obj_ref::<a href="#type-obj_ref">obj_ref()</a>) -&gt; <a href="#type-class">class()</a>
</code></pre>
<br />

retrieves the class record for a provided Object ID.

<a name="get_class-2"></a>

### get_class/2 ###

<pre><code>
get_class(Objects::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, ObjectId::<a href="#type-object_id">object_id()</a>) -&gt; <a href="#type-class">class()</a>
</code></pre>
<br />

retrieves the class record for a provided Object ID.

<a name="get_class_name-1"></a>

### get_class_name/1 ###

<pre><code>
get_class_name(Obj_ref::<a href="#type-obj_ref">obj_ref()</a>) -&gt; <a href="#type-class_name">class_name()</a>
</code></pre>
<br />

Equivalent to `get_class_name / 2`.

<a name="get_class_name-2"></a>

### get_class_name/2 ###

<pre><code>
get_class_name(Objects::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, ObjectId::<a href="#type-object_id">object_id()</a>) -&gt; <a href="#type-class_name">class_name()</a>
</code></pre>
<br />

retrieves the class name for a provided Object ID.

<a name="get_context-1"></a>

### get_context/1 ###

<pre><code>
get_context(Obj_ref::<a href="#type-obj_ref">obj_ref()</a>) -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

Equivalent to `get_context / 2`.

<a name="get_context-2"></a>

### get_context/2 ###

<pre><code>
get_context(Objects::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, ObjectId::<a href="#type-object_id">object_id()</a>) -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

retrieves the object context for an provided Object ID.

<a name="remove-2"></a>

### remove/2 ###

<pre><code>
remove(Context::<a href="#type-context">context()</a>, Obj_ref::<a href="#type-obj_ref">obj_ref()</a>) -&gt; ok
</code></pre>
<br />

Equivalent to `remove / 3`.

<a name="remove-3"></a>

### remove/3 ###

<pre><code>
remove(Context::<a href="#type-context">context()</a>, Objects::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, ObjectId::<a href="#type-object_id">object_id()</a>) -&gt; ok
</code></pre>
<br />

decreases the number of links for an object. If arrives to zero,
the object is removed.

<a name="remove_all-2"></a>

### remove_all/2 ###

<pre><code>
remove_all(Context::<a href="#type-context">context()</a>, Objects::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>) -&gt; ok
</code></pre>
<br />

removes all of the objects stored using objects_id.

<a name="set-2"></a>

### set/2 ###

<pre><code>
set(Obj_ref::<a href="#type-obj_ref">obj_ref()</a>, Object::<a href="#type-ephp_object">ephp_object()</a>) -&gt; ok
</code></pre>
<br />

stores an object given the Object ID.

<a name="set-3"></a>

### set/3 ###

<pre><code>
set(Objects::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, ObjectId::<a href="#type-object_id">object_id()</a>, Object::<a href="#type-ephp_object">ephp_object()</a>) -&gt; ok
</code></pre>
<br />

stores an object given the Object ID.

<a name="set_class-2"></a>

### set_class/2 ###

<pre><code>
set_class(Obj_ref::<a href="ephp.md#type-obj_ref">ephp:obj_ref()</a>, Class::<a href="#type-class">class()</a>) -&gt; ok
</code></pre>
<br />

set a class for an existent object.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, <a href="ephp.md#type-objects_id">ephp:objects_id()</a>}
</code></pre>
<br />

creates a new Objects storage.

