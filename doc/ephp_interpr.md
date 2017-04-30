

# Module ephp_interpr #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-break">break()</a> ###


<pre><code>
break() = break | {break, pos_integer()}
</code></pre>




### <a name="type-flow_status">flow_status()</a> ###


<pre><code>
flow_status() = <a href="#type-break">break()</a> | continue | <a href="#type-return">return()</a> | false
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#process-2">process/2</a></td><td></td></tr><tr><td valign="top"><a href="#run-2">run/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="process-2"></a>

### process/2 ###

<pre><code>
process(Context::<a href="#type-context">context()</a>, Statements::[<a href="#type-main_statement">main_statement()</a>]) -&gt; {ok, binary() | <a href="#type-return">return()</a> | false}
</code></pre>
<br />

<a name="run-2"></a>

### run/2 ###

<pre><code>
run(Context::<a href="#type-context">context()</a>, Print_text::<a href="#type-main_statement">main_statement()</a>) -&gt; <a href="#type-flow_status">flow_status()</a>
</code></pre>
<br />

