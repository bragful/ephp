

# Module ephp_array #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

The PHP Array is a collection that could be used as a simple array
or a hash.

<a name="description"></a>

## Description ##

This module helps to create an structure to handle the
PHP Array.

An example for the use of this module:

```erlang

       Array0 = ephp_array:new().
       Array1 = ephp_array:store(auto, <<"hello world!">>).
       Array2 = ephp_array:store(auto, <<"bye!">>).
       ArrayN = ephp_array:from_list([1, 2, 3, 4, 5]).
```

It's possible to link the actions on the array to a specific module
and function. This is useful to get the updates for an array in
real-time.

The message received are the followings:

* `[Array, {retrieve, Key}]` get the information from
the backend.

* `[Array, {add, Key, Value}]` add the key with the value for the
array in the backend.

* `[Array, {update, Key, Value}]` change the value for the key in
the array in the backend.

* `[Array, {remove, Key}]` removes the value for the key from the
array in the backend.

* `[Array, {map, Fun}]` run the map function for the array in the
backend.

* `[Array, {fold, Fun, Initial}]` run the fold function for the
array in the backend.

* `[Array, to_list]` transfor the array in a property list from
the array.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#erase-2">erase/2</a></td><td>removes an element from the array given the index.</td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td>finds an element by the key passed as a param.</td></tr><tr><td valign="top"><a href="#find-3">find/3</a></td><td>finds an element by the passed as a param.</td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td>performs a fold on all of the elements in the array given an initial
value and changing that value in each element.</td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td>transform the list passed as param in a PHP Array.</td></tr><tr><td valign="top"><a href="#map-2">map/2</a></td><td>performs a map action on all of the elemnts in the array.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>creates an empty PHP Array structure.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>creates an empty PHP Array structure linked to a module and
function.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>retrieve the size of the array.</td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td>stores a new element given a key and a value.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>transform a PHP Array to a property list.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="erase-2"></a>

### erase/2 ###

<pre><code>
erase(Key::<a href="#type-mixed">mixed()</a>, Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

removes an element from the array given the index.

<a name="find-2"></a>

### find/2 ###

<pre><code>
find(Key::<a href="#type-mixed">mixed()</a>, Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; {ok, <a href="#type-mixed">mixed()</a>} | error
</code></pre>
<br />

finds an element by the key passed as a param.

<a name="find-3"></a>

### find/3 ###

<pre><code>
find(Key::<a href="#type-mixed">mixed()</a>, Array::<a href="#type-ephp_array">ephp_array()</a>, Default::<a href="#type-mixed">mixed()</a>) -&gt; <a href="#type-mixed">mixed()</a>
</code></pre>
<br />

finds an element by the passed as a param. If the value isn't found the
default value passed as param is returned.

<a name="fold-3"></a>

### fold/3 ###

<pre><code>
fold(Fun::function(), Initial::<a href="#type-mixed">mixed()</a>, Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; <a href="#type-mixed">mixed()</a>
</code></pre>
<br />

performs a fold on all of the elements in the array given an initial
value and changing that value in each element.

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(List::[<a href="#type-mixed">mixed()</a>]) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

transform the list passed as param in a PHP Array.

<a name="map-2"></a>

### map/2 ###

<pre><code>
map(Fun::function(), Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

performs a map action on all of the elemnts in the array.

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

creates an empty PHP Array structure.

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Module::module(), Function::function(), Args::[any()]) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

creates an empty PHP Array structure linked to a module and
function. To handle the internal information.

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

retrieve the size of the array.

<a name="store-3"></a>

### store/3 ###

<pre><code>
store(Key::auto | <a href="#type-mixed">mixed()</a>, Value::<a href="#type-mixed">mixed()</a>, Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

stores a new element given a key and a value. If the key passed is
`auto` the key is autogenerated based on the last numeric index
used.

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; [<a href="#type-mixed">mixed()</a>]
</code></pre>
<br />

transform a PHP Array to a property list.

