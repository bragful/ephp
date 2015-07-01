

# Module ephp #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-values">values()</a> ###



<pre><code>
values() = integer() | binary() | float() | '?DICT_TYPE'
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#context_new-0">context_new/0</a></td><td></td></tr><tr><td valign="top"><a href="#context_new-1">context_new/1</a></td><td></td></tr><tr><td valign="top"><a href="#eval-2">eval/2</a></td><td></td></tr><tr><td valign="top"><a href="#eval-3">eval/3</a></td><td></td></tr><tr><td valign="top"><a href="#main-1">main/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_func-5">register_func/5</a></td><td></td></tr><tr><td valign="top"><a href="#register_module-2">register_module/2</a></td><td></td></tr><tr><td valign="top"><a href="#register_var-3">register_var/3</a></td><td></td></tr><tr><td valign="top"><a href="#run-2">run/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

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


<a name="eval-2"></a>

### eval/2 ###


<pre><code>
eval(Context::<a href="#type-context">context()</a>, PHP::string() | binary()) -&gt; {ok, Result::binary()} | {error, Reason::<a href="#type-reason">reason()</a>} | {error, {Code::binary(), Line::integer(), Col::integer()}}
</code></pre>
<br />


<a name="eval-3"></a>

### eval/3 ###


<pre><code>
eval(Filename::binary(), Context::<a href="#type-context">context()</a>, PHP::string() | binary()) -&gt; {ok, Result::binary()} | {error, Reason::<a href="#type-reason">reason()</a>} | {error, {Code::binary(), Line::integer(), Col::integer()}}
</code></pre>
<br />


<a name="main-1"></a>

### main/1 ###


<pre><code>
main(Args::[string()]) -&gt; integer()
</code></pre>
<br />


<a name="register_func-5"></a>

### register_func/5 ###


<pre><code>
register_func(Ctx::<a href="#type-context">context()</a>, PHPName::binary(), Module::atom(), Fun::atom(), PackArgs::boolean()) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
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


<a name="start-0"></a>

### start/0 ###


<pre><code>
start() -&gt; ok
</code></pre>
<br />


