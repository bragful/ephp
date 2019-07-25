

# Module ephp_shutdown #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#destroy-1">destroy/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_funcs-1">get_funcs/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_func-3">register_func/3</a></td><td></td></tr><tr><td valign="top"><a href="#shutdown-1">shutdown/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#unregister_func-3">unregister_func/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="destroy-1"></a>

### destroy/1 ###

<pre><code>
destroy(Ref::<a href="ephp.md#type-shutdown_id">ephp:shutdown_id()</a>) -&gt; ok
</code></pre>
<br />

<a name="get_funcs-1"></a>

### get_funcs/1 ###

<pre><code>
get_funcs(Ref::reference()) -&gt; [<a href="#type-callable">callable()</a>]
</code></pre>
<br />

<a name="register_func-3"></a>

### register_func/3 ###

<pre><code>
register_func(Ref::reference(), NS::<a href="#type-namespace">namespace()</a>, FuncName::<a href="#type-callable">callable()</a>) -&gt; ok
</code></pre>
<br />

<a name="shutdown-1"></a>

### shutdown/1 ###

<pre><code>
shutdown(Context::<a href="#type-context">context()</a>) -&gt; undefined
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, reference()}
</code></pre>
<br />

<a name="unregister_func-3"></a>

### unregister_func/3 ###

<pre><code>
unregister_func(Ref::reference(), NS::<a href="#type-namespace">namespace()</a>, FuncName::<a href="#type-callable">callable()</a>) -&gt; ok
</code></pre>
<br />

