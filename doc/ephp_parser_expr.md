

# Module ephp_parser_expr #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-associativity">associativity()</a> ###


<pre><code>
associativity() = no_assoc | left | right
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_op-2">add_op/2</a></td><td></td></tr><tr><td valign="top"><a href="#expression-3">expression/3</a></td><td></td></tr><tr><td valign="top"><a href="#precedence-1">precedence/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_op-2"></a>

### add_op/2 ###

`add_op(Add, Parsed) -> any()`

<a name="expression-3"></a>

### expression/3 ###

<pre><code>
expression(Text::binary(), Parser::<a href="#type-parser">parser()</a>, Parsed::[<a href="#type-expression">expression()</a>]) -&gt; {binary(), <a href="#type-parser">parser()</a>, [<a href="#type-expression">expression()</a>] | <a href="#type-expression">expression()</a>}
</code></pre>
<br />

<a name="precedence-1"></a>

### precedence/1 ###

<pre><code>
precedence(X1::binary()) -&gt; {<a href="#type-associativity">associativity()</a>, pos_integer()} | false
</code></pre>
<br />

