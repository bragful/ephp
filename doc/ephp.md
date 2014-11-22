

# Module ephp #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-values">values()</a> ###



<pre><code>
values() = integer() | binary() | float() | dict()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compile-1">compile/1</a></td><td></td></tr><tr><td valign="top"><a href="#context_new-0">context_new/0</a></td><td></td></tr><tr><td valign="top"><a href="#context_new-1">context_new/1</a></td><td></td></tr><tr><td valign="top"><a href="#context_new-2">context_new/2</a></td><td></td></tr><tr><td valign="top"><a href="#eval-2">eval/2</a></td><td></td></tr><tr><td valign="top"><a href="#main-1">main/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_fun-4">register_fun/4</a></td><td></td></tr><tr><td valign="top"><a href="#register_module-2">register_module/2</a></td><td></td></tr><tr><td valign="top"><a href="#register_var-3">register_var/3</a></td><td></td></tr><tr><td valign="top"><a href="#run-2">run/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compile-1"></a>

### compile/1 ###


<pre><code>
compile(PHP::binary() | string()) -&gt; {ok, Result::[<a href="#type-statement">statement()</a>]} | {error, Reason::<a href="#type-reason">reason()</a>} | {error, {Code::binary(), Line::integer(), Col::integer()}}
</code></pre>
<br />


<a name="context_new-0"></a>

### context_new/0 ###


<pre><code>
context_new() -&gt; {ok, <a href="#type-context">context()</a>} | {error, Reason::term()}
</code></pre>
<br />


<a name="context_new-1"></a>

### context_new/1 ###


<pre><code>
context_new(Filename::binary()) -&gt; {ok, <a href="#type-context">context()</a>} | {error, Reason::term()}
</code></pre>
<br />


<a name="context_new-2"></a>

### context_new/2 ###


<pre><code>
context_new(Filename::binary(), Dirname::binary()) -&gt; {ok, <a href="#type-context">context()</a>} | {error, Reason::term()}
</code></pre>
<br />


<a name="eval-2"></a>

### eval/2 ###


<pre><code>
eval(Context::<a href="#type-context">context()</a>, PHP::string() | binary()) -&gt; {ok, Result::binary()} | {error, Reason::<a href="#type-reason">reason()</a>} | {error, {Code::binary(), Line::integer(), Col::integer()}}
</code></pre>
<br />


<a name="main-1"></a>

### main/1 ###


<pre><code>
main(Args::[string()]) -&gt; integer()
</code></pre>
<br />


<a name="register_fun-4"></a>

### register_fun/4 ###


<pre><code>
register_fun(Ctx::<a href="#type-context">context()</a>, PHPName::binary(), Module::atom(), Fun::atom()) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />


<a name="register_module-2"></a>

### register_module/2 ###


<pre><code>
register_module(Ctx::<a href="#type-context">context()</a>, Module::atom()) -&gt; ok
</code></pre>
<br />


<a name="register_var-3"></a>

### register_var/3 ###


<pre><code>
register_var(Ctx::<a href="#type-context">context()</a>, Var::binary(), Value::<a href="#type-values">values()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />


<a name="run-2"></a>

### run/2 ###


<pre><code>
run(Context::<a href="#type-context">context()</a>, Compiled::[<a href="#type-statement">statement()</a>]) -&gt; {ok, binary()} | {error, Reason::<a href="#type-reason">reason()</a>}
</code></pre>
<br />


