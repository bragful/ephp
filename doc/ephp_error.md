

# Module ephp_error #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-error_type">error_type()</a> ###



<pre><code>
error_type() = eundefclass | ebadbnot | erequired | einclude | enofile | eundefun | earrayundef | eundeftoken | enoclassscope | emethodtypes | eundefmethod | edivzero | eparse | enostatement | eunknownst
</code></pre>





### <a name="type-throw_error">throw_error()</a> ###



<pre><code>
throw_error() = atom() | {error, <a href="#type-error_type">error_type()</a>, <a href="#type-line">line()</a>, binary()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#error-1">error/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_error-2">handle_error/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="error-1"></a>

### error/1 ###


<pre><code>
error(X1::<a href="#type-throw_error">throw_error()</a>) -&gt; ok
</code></pre>
<br />


<a name="handle_error-2"></a>

### handle_error/2 ###


<pre><code>
handle_error(Context::<a href="#type-context">context()</a>, X2::{error, <a href="#type-error_type">error_type()</a>, <a href="#type-line">line()</a>, binary()}) -&gt; ok
</code></pre>
<br />


