

# Module ephp_array #
* [Description](#description)
* [Data Types](#types)
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
       Array1 = ephp_array:store(auto, <<"hello world!">>, Array0).
       Array2 = ephp_array:store(auto, <<"bye!">>, Array1).
       ArrayN = ephp_array:from_list([1, 2, 3, 4, 5]).
```

<a name="types"></a>

## Data Types ##




### <a name="type-ephp_array">ephp_array()</a> ###


<pre><code>
ephp_array() = #ephp_array{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#current-1">current/1</a></td><td>retrieves the element under the cursor.</td></tr><tr><td valign="top"><a href="#cursor-2">cursor/2</a></td><td>set the cursor for an array.</td></tr><tr><td valign="top"><a href="#erase-2">erase/2</a></td><td>removes an element from the array given the index.</td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td>finds an element by the key passed as a param.</td></tr><tr><td valign="top"><a href="#find-3">find/3</a></td><td>finds an element by the passed as a param.</td></tr><tr><td valign="top"><a href="#first-1">first/1</a></td><td>moves the cursor to the begin of the array and retrieves that element.</td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td>performs a fold on all of the elements in the array given an initial
value and changing that value in each element.</td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td>transform the list passed as param in a PHP Array.</td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>transform the map passed as param in a PHP Array.</td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td>returns an array only with the keys.</td></tr><tr><td valign="top"><a href="#ksort-2">ksort/2</a></td><td>sort an array based on incoming flags (as a second param).</td></tr><tr><td valign="top"><a href="#last-1">last/1</a></td><td>moves the cursor to the end of the array and retrieves that element.</td></tr><tr><td valign="top"><a href="#map-2">map/2</a></td><td>performs a map action on all of the elemnts in the array.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>creates an empty PHP Array structure.</td></tr><tr><td valign="top"><a href="#next-1">next/1</a></td><td>moves the cursor the to next element and retrieves that element.</td></tr><tr><td valign="top"><a href="#pop-1">pop/1</a></td><td>returns a tuple with the value poped from the array (last element) and
the rest of the array.</td></tr><tr><td valign="top"><a href="#prev-1">prev/1</a></td><td>moves the cursor to the previous element and retrieves that element.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>retrieve the size of the array.</td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td>stores a new element given a key and a value.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>transform a PHP Array to a property list.</td></tr><tr><td valign="top"><a href="#update_counter-3">update_counter/3</a></td><td>creates or updates an element increasing the number passed as second
parameter and return the modified array.</td></tr><tr><td valign="top"><a href="#values-1">values/1</a></td><td>returns list only with the values (no keys).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="current-1"></a>

### current/1 ###

<pre><code>
current(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; {ok, {<a href="#type-mixed">mixed()</a>, <a href="#type-mixed">mixed()</a>}} | {error, empty | enocursor}
</code></pre>
<br />

retrieves the element under the cursor.

<a name="cursor-2"></a>

### cursor/2 ###

<pre><code>
cursor(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>, Cursor::pos_integer() | false) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

set the cursor for an array.

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
find(Key::<a href="#type-mixed">mixed()</a>, Array::<a href="#type-ephp_array">ephp_array()</a>, Default::any()) -&gt; any()
</code></pre>
<br />

finds an element by the passed as a param. If the value isn't found the
default value passed as param is returned.

<a name="first-1"></a>

### first/1 ###

<pre><code>
first(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; {ok, {<a href="#type-mixed">mixed()</a>, <a href="#type-mixed">mixed()</a>}, <a href="#type-ephp_array">ephp_array()</a>} | {error, empty}
</code></pre>
<br />

moves the cursor to the begin of the array and retrieves that element.

<a name="fold-3"></a>

### fold/3 ###

<pre><code>
fold(Fun::function(), Initial::any(), Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; any()
</code></pre>
<br />

performs a fold on all of the elements in the array given an initial
value and changing that value in each element.

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(List::[any()]) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

transform the list passed as param in a PHP Array.

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::map()) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

transform the map passed as param in a PHP Array.

<a name="keys-1"></a>

### keys/1 ###

<pre><code>
keys(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

returns an array only with the keys.

<a name="ksort-2"></a>

### ksort/2 ###

<pre><code>
ksort(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>, Flags::pos_integer()) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

sort an array based on incoming flags (as a second param).

<a name="last-1"></a>

### last/1 ###

<pre><code>
last(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; {ok, {<a href="#type-mixed">mixed()</a>, <a href="#type-mixed">mixed()</a>}, <a href="#type-ephp_array">ephp_array()</a>} | {error, empty}
</code></pre>
<br />

moves the cursor to the end of the array and retrieves that element.

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

<a name="next-1"></a>

### next/1 ###

<pre><code>
next(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; {ok, {<a href="#type-mixed">mixed()</a>, <a href="#type-mixed">mixed()</a>}, <a href="#type-ephp_array">ephp_array()</a>} | {error, eof | empty | enocursor}
</code></pre>
<br />

moves the cursor the to next element and retrieves that element.

<a name="pop-1"></a>

### pop/1 ###

<pre><code>
pop(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; {<a href="#type-mixed">mixed()</a>, <a href="#type-ephp_array">ephp_array()</a>}
</code></pre>
<br />

returns a tuple with the value poped from the array (last element) and
the rest of the array.

<a name="prev-1"></a>

### prev/1 ###

<pre><code>
prev(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; {ok, {<a href="#type-mixed">mixed()</a>, <a href="#type-mixed">mixed()</a>}, <a href="#type-ephp_array">ephp_array()</a>} | {error, bof | empty | enocursor}
</code></pre>
<br />

moves the cursor to the previous element and retrieves that element.

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
store(Key::auto | non_neg_integer() | <a href="#type-mixed">mixed()</a>, Value::<a href="#type-mixed">mixed()</a>, Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

stores a new element given a key and a value. If the key passed is
`auto` the key is autogenerated based on the last numeric index
used.

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; [{<a href="#type-mixed">mixed()</a>, <a href="#type-mixed">mixed()</a>}]
</code></pre>
<br />

transform a PHP Array to a property list.

<a name="update_counter-3"></a>

### update_counter/3 ###

<pre><code>
update_counter(Key::<a href="#type-mixed">mixed()</a>, Increase::integer(), Array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

creates or updates an element increasing the number passed as second
parameter and return the modified array.

<a name="values-1"></a>

### values/1 ###

<pre><code>
values(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; [<a href="#type-mixed">mixed()</a>]
</code></pre>
<br />

returns list only with the values (no keys).

