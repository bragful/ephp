

# Module ephp_config #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_atom-1">get_atom/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_bool-1">get_bool/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_bool-2">get_bool/2</a></td><td></td></tr><tr><td valign="top"><a href="#module_init-1">module_init/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_config-1">read_config/1</a></td><td></td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_local-0">start_local/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop_local-0">stop_local/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(Key::binary()) -&gt; <a href="#type-mixed">mixed()</a>
</code></pre>
<br />

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Key::binary(), Default::<a href="#type-mixed">mixed()</a>) -&gt; <a href="#type-mixed">mixed()</a>
</code></pre>
<br />

<a name="get_atom-1"></a>

### get_atom/1 ###

<pre><code>
get_atom(Key::binary()) -&gt; atom()
</code></pre>
<br />

<a name="get_bool-1"></a>

### get_bool/1 ###

<pre><code>
get_bool(Key::binary()) -&gt; boolean()
</code></pre>
<br />

<a name="get_bool-2"></a>

### get_bool/2 ###

<pre><code>
get_bool(Key::binary(), Default::boolean()) -&gt; boolean()
</code></pre>
<br />

<a name="module_init-1"></a>

### module_init/1 ###

<pre><code>
module_init(Module::module()) -&gt; ok
</code></pre>
<br />

<a name="read_config-1"></a>

### read_config/1 ###

<pre><code>
read_config(File::<a href="#type-file_name">file_name()</a>) -&gt; <a href="proplists.md#type-proplists">proplists:proplists()</a>
</code></pre>
<br />

<a name="set-2"></a>

### set/2 ###

<pre><code>
set(Key::binary(), Value::<a href="#type-mixed">mixed()</a>) -&gt; ok
</code></pre>
<br />

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(File::<a href="#type-file_name">file_name()</a>) -&gt; ok
</code></pre>
<br />

<a name="start_local-0"></a>

### start_local/0 ###

<pre><code>
start_local() -&gt; ok
</code></pre>
<br />

<a name="stop_local-0"></a>

### stop_local/0 ###

<pre><code>
stop_local() -&gt; ok
</code></pre>
<br />

