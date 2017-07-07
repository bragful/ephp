

# Module ephp_array #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#erase-2">erase/2</a></td><td></td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td></td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td></td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td></td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="erase-2"></a>

### erase/2 ###

<pre><code>
erase(Key::<a href="#type-mixed">mixed()</a>, Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="find-2"></a>

### find/2 ###

<pre><code>
find(Key::<a href="#type-mixed">mixed()</a>, Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; {ok, <a href="#type-mixed">mixed()</a>} | error
</code></pre>
<br />

<a name="fold-3"></a>

### fold/3 ###

<pre><code>
fold(Fun::function(), Initial::<a href="#type-mixed">mixed()</a>, Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; <a href="#type-mixed">mixed()</a>
</code></pre>
<br />

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(List::[<a href="#type-mixed">mixed()</a>]) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Module::module(), Function::function(), Args::[any()]) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="store-3"></a>

### store/3 ###

<pre><code>
store(Key::auto | <a href="#type-mixed">mixed()</a>, Value::<a href="#type-mixed">mixed()</a>, Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(Ephp_array::<a href="#type-ephp_array">ephp_array()</a>) -&gt; [<a href="#type-mixed">mixed()</a>]
</code></pre>
<br />

