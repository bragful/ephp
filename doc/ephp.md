

# Module ephp #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

The ephp module is in charge to give an easy way to create the context
and other actions to be performed from the project where ephp is
included mainly to run the PHP code.

<a name="description"></a>

## Description ##

The easy way to use is:

```erlang

       {ok, Ctx} = ephp:context_new(),
       PHP = "<? $a = 5 * 23; ?>Result for $a = <?=$a?>",
       {ok, Text} = ephp:eval(Ctx, PHP).
```

This module is in use for PHP script, contains the `main/1` function
to run from console.
<a name="types"></a>

## Data Types ##




### <a name="type-classes_id">classes_id()</a> ###


<pre><code>
classes_id() = reference()
</code></pre>




### <a name="type-consts_id">consts_id()</a> ###


<pre><code>
consts_id() = reference()
</code></pre>




### <a name="type-context_id">context_id()</a> ###


<pre><code>
context_id() = reference()
</code></pre>




### <a name="type-errors_id">errors_id()</a> ###


<pre><code>
errors_id() = reference()
</code></pre>




### <a name="type-eval_return">eval_return()</a> ###


<pre><code>
eval_return() = {ok, Result::<a href="ephp_interpr.md#type-flow_status">ephp_interpr:flow_status()</a>} | {error, <a href="#type-reason">reason()</a>, <a href="#type-line">line()</a>, File::binary(), <a href="#type-error_level">error_level()</a>, Data::any()}
</code></pre>




### <a name="type-funcs_id">funcs_id()</a> ###


<pre><code>
funcs_id() = reference()
</code></pre>




### <a name="type-includes_id">includes_id()</a> ###


<pre><code>
includes_id() = reference()
</code></pre>




### <a name="type-objects_id">objects_id()</a> ###


<pre><code>
objects_id() = reference()
</code></pre>




### <a name="type-output_id">output_id()</a> ###


<pre><code>
output_id() = reference()
</code></pre>




### <a name="type-shutdown_id">shutdown_id()</a> ###


<pre><code>
shutdown_id() = reference()
</code></pre>




### <a name="type-values">values()</a> ###


<pre><code>
values() = integer() | binary() | float() | <a href="#type-ephp_array">ephp_array()</a>
</code></pre>




### <a name="type-vars_id">vars_id()</a> ###


<pre><code>
vars_id() = reference()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#context_new-0">context_new/0</a></td><td>creates a new context using <code>-</code> as script name.</td></tr><tr><td valign="top"><a href="#context_new-1">context_new/1</a></td><td>creates a new context passing <code>Filename</code> as param.</td></tr><tr><td valign="top"><a href="#eval-2">eval/2</a></td><td>eval PHP code in a context passed as params.</td></tr><tr><td valign="top"><a href="#eval-3">eval/3</a></td><td>adds the <code>Filename</code> to configure properly the <code>__FILE__</code> and <code>__DIR__</code>
constants and evaluates the code for the third parameter.</td></tr><tr><td valign="top"><a href="#main-1">main/1</a></td><td>called from script passing the name of the filename to be run or
nothing to show the help message.</td></tr><tr><td valign="top"><a href="#register_func-6">register_func/6</a></td><td>register function in a context passed as a param.</td></tr><tr><td valign="top"><a href="#register_func-7">register_func/7</a></td><td>register function in a context passed as a param.</td></tr><tr><td valign="top"><a href="#register_module-2">register_module/2</a></td><td>register a module.</td></tr><tr><td valign="top"><a href="#register_superglobals-2">register_superglobals/2</a></td><td>register the superglobals variables in the context passed as param.</td></tr><tr><td valign="top"><a href="#register_var-3">register_var/3</a></td><td>register a variable with a value in the context passed as param.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>function to ensure all of the applications and the base configuration
is set properly before use ephp.</td></tr><tr><td valign="top"><a href="#stop_cover-0">stop_cover/0</a></td><td>stops the cover system.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="context_new-0"></a>

### context_new/0 ###

<pre><code>
context_new() -&gt; {ok, <a href="#type-context">context()</a>}
</code></pre>
<br />

creates a new context using `-` as script name.

<a name="context_new-1"></a>

### context_new/1 ###

<pre><code>
context_new(Filename::binary()) -&gt; {ok, <a href="#type-context">context()</a>}
</code></pre>
<br />

creates a new context passing `Filename` as param.

<a name="eval-2"></a>

### eval/2 ###

<pre><code>
eval(Context::<a href="#type-context">context()</a>, PHP::string() | binary()) -&gt; <a href="#type-eval_return">eval_return()</a>
</code></pre>
<br />

eval PHP code in a context passed as params.

<a name="eval-3"></a>

### eval/3 ###

<pre><code>
eval(Filename::binary(), Context::<a href="#type-context">context()</a>, PHP::string() | binary() | [term()]) -&gt; <a href="#type-eval_return">eval_return()</a>
</code></pre>
<br />

Equivalent to `eval / 2`.

adds the `Filename` to configure properly the `__FILE__` and `__DIR__`
constants and evaluates the code for the third parameter. This parameter
could contents a binary text with PHP code or a parsed PHP content.

<a name="main-1"></a>

### main/1 ###

<pre><code>
main(Args::[string()]) -&gt; integer()
</code></pre>
<br />

called from script passing the name of the filename to be run or
nothing to show the help message.

<a name="register_func-6"></a>

### register_func/6 ###

<pre><code>
register_func(Ctx::<a href="#type-context">context()</a>, PHPName::binary(), Module::module(), Fun::atom(), PackArgs::boolean(), Args::<a href="ephp_func.md#type-validation_args">ephp_func:validation_args()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

register function in a context passed as a param. The params to be
sent are the PHP function name, the module, function name and args
in the Erlang side.

Other param is about if the params should be packed or not. That means
the args could be sent one by one or as only one in an array.

<a name="register_func-7"></a>

### register_func/7 ###

<pre><code>
register_func(Ctx::<a href="#type-context">context()</a>, NS::<a href="#type-namespace">namespace()</a>, PHPName::binary(), Module::module(), Fun::atom(), PackArgs::boolean(), Args::<a href="ephp_func.md#type-validation_args">ephp_func:validation_args()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

register function in a context passed as a param. The params to be
sent are the PHP function name, the module, function name and args
in the Erlang side.

Other param is about if the params should be packed or not. That means
the args could be sent one by one or as only one in an array.

<a name="register_module-2"></a>

### register_module/2 ###

<pre><code>
register_module(Ctx::<a href="#type-context">context()</a>, Module::module()) -&gt; ok
</code></pre>
<br />

register a module.

__See also:__ [ephp_func](ephp_func.md).

<a name="register_superglobals-2"></a>

### register_superglobals/2 ###

<pre><code>
register_superglobals(Ctx::<a href="#type-context">context()</a>, RawArgs::[string()]) -&gt; ok
</code></pre>
<br />

register the superglobals variables in the context passed as param.

<a name="register_var-3"></a>

### register_var/3 ###

<pre><code>
register_var(Ctx::<a href="#type-context">context()</a>, Var::binary(), Value::<a href="#type-values">values()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

register a variable with a value in the context passed as param.

<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; ok
</code></pre>
<br />

function to ensure all of the applications and the base configuration
is set properly before use ephp.

<a name="stop_cover-0"></a>

### stop_cover/0 ###

<pre><code>
stop_cover() -&gt; ok
</code></pre>
<br />

stops the cover system.

