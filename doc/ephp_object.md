

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
Object ID where it was stored.</td></tr><tr><td valign="top"><a href="#add_link-1">add_link/1</a></td><td>increases the number of links for an object.</td></tr><tr><td valign="top"><a href="#add_link-2">add_link/2</a></td><td>increases the number of links for an object.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>retrieves an object based on the Object ID.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>retrieves an object based on the Object ID.</td></tr><tr><td valign="top"><a href="#get_class_name-1">get_class_name/1</a></td><td>retrieves the class name for an provided Object ID.</td></tr><tr><td valign="top"><a href="#get_class_name-2">get_class_name/2</a></td><td>retrieves the class name for an provided Object ID.</td></tr><tr><td valign="top"><a href="#get_context-1">get_context/1</a></td><td>retrieves the object context for an provided Object ID.</td></tr><tr><td valign="top"><a href="#get_context-2">get_context/2</a></td><td>retrieves the object context for an provided Object ID.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>removes an object from the storage given the object or its ID.</td></tr><tr><td valign="top"><a href="#remove-3">remove/3</a></td><td>removes an object from the storage given the object or its ID.</td></tr><tr><td valign="top"><a href="#remove_all-2">remove_all/2</a></td><td>removes all of the objects stored using objects_id.</td></tr><tr><td valign="top"><a href="#remove_link-2">remove_link/2</a></td><td>decreases the number of links for an object.</td></tr><tr><td valign="top"><a href="#remove_link-3">remove_link/3</a></td><td>decreases the number of links for an object.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>stores an object given the Object ID.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>creates a new Objects storage.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-2"></a>

### add/2 ###

<pre><code>
add(Ref::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, Object::<a href="#type-ephp_object">ephp_object()</a>) -&gt; <a href="#type-object_id">object_id()</a>
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

increases the number of links for an object.

<a name="add_link-2"></a>

### add_link/2 ###

<pre><code>
add_link(Ref::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, ObjectId::<a href="#type-object_id">object_id()</a>) -&gt; ok
</code></pre>
<br />

increases the number of links for an object.

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

<a name="get_class_name-1"></a>

### get_class_name/1 ###

<pre><code>
get_class_name(Obj_ref::<a href="#type-obj_ref">obj_ref()</a>) -&gt; <a href="#type-class_name">class_name()</a>
</code></pre>
<br />

retrieves the class name for an provided Object ID.

<a name="get_class_name-2"></a>

### get_class_name/2 ###

<pre><code>
get_class_name(Ref::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, ObjectId::<a href="#type-object_id">object_id()</a>) -&gt; <a href="#type-class_name">class_name()</a>
</code></pre>
<br />

retrieves the class name for an provided Object ID.

<a name="get_context-1"></a>

### get_context/1 ###

<pre><code>
get_context(Obj_ref::<a href="#type-obj_ref">obj_ref()</a>) -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

retrieves the object context for an provided Object ID.

<a name="get_context-2"></a>

### get_context/2 ###

<pre><code>
get_context(Ref::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, ObjectId::<a href="#type-object_id">object_id()</a>) -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

retrieves the object context for an provided Object ID.

<a name="remove-2"></a>

### remove/2 ###

<pre><code>
remove(Context::<a href="#type-context">context()</a>, Obj_ref::<a href="#type-obj_ref">obj_ref()</a> | <a href="#type-object_id">object_id()</a>) -&gt; ok
</code></pre>
<br />

removes an object from the storage given the object or its ID.

<a name="remove-3"></a>

### remove/3 ###

<pre><code>
remove(Context::<a href="#type-context">context()</a>, Ref::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, Ephp_object::<a href="#type-ephp_object">ephp_object()</a> | <a href="#type-object_id">object_id()</a>) -&gt; ok
</code></pre>
<br />

removes an object from the storage given the object or its ID.

<a name="remove_all-2"></a>

### remove_all/2 ###

<pre><code>
remove_all(Context::<a href="#type-context">context()</a>, Ref::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>) -&gt; ok
</code></pre>
<br />

removes all of the objects stored using objects_id.

<a name="remove_link-2"></a>

### remove_link/2 ###

<pre><code>
remove_link(Context::<a href="#type-context">context()</a>, Obj_ref::<a href="#type-obj_ref">obj_ref()</a>) -&gt; ok
</code></pre>
<br />

decreases the number of links for an object. If arrives to zero,
the object is removed.

<a name="remove_link-3"></a>

### remove_link/3 ###

<pre><code>
remove_link(Context::<a href="#type-context">context()</a>, Ref::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, ObjectId::<a href="#type-object_id">object_id()</a>) -&gt; ok
</code></pre>
<br />

decreases the number of links for an object. If arrives to zero,
the object is removed.

<a name="set-3"></a>

### set/3 ###

<pre><code>
set(Ref::<a href="ephp.md#type-objects_id">ephp:objects_id()</a>, ObjectId::<a href="#type-object_id">object_id()</a>, Object::<a href="#type-ephp_object">ephp_object()</a>) -&gt; ok
</code></pre>
<br />

stores an object given the Object ID.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, <a href="ephp.md#type-objects_id">ephp:objects_id()</a>}
</code></pre>
<br />

creates a new Objects storage.

