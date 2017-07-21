

# Module ephp_mem #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-mem_id">mem_id()</a> ###


<pre><code>
mem_id() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-1">add/1</a></td><td>adds information for the storage and returns the MemId to access later
to that information.</td></tr><tr><td valign="top"><a href="#add_link-1">add_link/1</a></td><td>increase the links attribute for a specific MemId.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>get the content for a specific MemId.</td></tr><tr><td valign="top"><a href="#remove-1">remove/1</a></td><td>removes an entry given by MemId in the storage data.</td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td>stores the element in the position required.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>starts the memory storage system for referenced data.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>remove the information about the links for the referenced data.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-1"></a>

### add/1 ###

<pre><code>
add(Data::any()) -&gt; <a href="#type-mem_id">mem_id()</a>
</code></pre>
<br />

adds information for the storage and returns the MemId to access later
to that information.

<a name="add_link-1"></a>

### add_link/1 ###

<pre><code>
add_link(MemId::<a href="#type-mem_id">mem_id()</a>) -&gt; ok
</code></pre>
<br />

increase the links attribute for a specific MemId.

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(MemId::<a href="#type-mem_id">mem_id()</a>) -&gt; any()
</code></pre>
<br />

get the content for a specific MemId.

<a name="remove-1"></a>

### remove/1 ###

<pre><code>
remove(MemId::<a href="#type-mem_id">mem_id()</a>) -&gt; ok
</code></pre>
<br />

removes an entry given by MemId in the storage data.

<a name="set-2"></a>

### set/2 ###

<pre><code>
set(MemId::<a href="#type-mem_id">mem_id()</a>, Data::any()) -&gt; ok
</code></pre>
<br />

stores the element in the position required.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, module()}
</code></pre>
<br />

starts the memory storage system for referenced data.

<a name="stop-0"></a>

### stop/0 ###

<pre><code>
stop() -&gt; ok
</code></pre>
<br />

remove the information about the links for the referenced data.

