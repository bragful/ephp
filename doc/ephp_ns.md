

# Module ephp_ns #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module stores handle the way to use namespaces.

<a name="description"></a>

## Description ##
The namespaces
are needed mainly during parsing (compilation-time) but dynamically
it's sometimes needed as well.
<a name="types"></a>

## Data Types ##




### <a name="type-namespace">namespace()</a> ###


<pre><code>
namespace() = [binary()]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#find-2">find/2</a></td><td>find a matching namespace based on the initial match.</td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td>join two namespaces to generate only one, checking for absolute and relative.</td></tr><tr><td valign="top"><a href="#normalize-1">normalize/1</a></td><td>normalize namespace removing initial empty (absolute) if any.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>converts a string into a 2-tuple: namespace and class name.</td></tr><tr><td valign="top"><a href="#split-1">split/1</a></td><td>retrieve last part of the namespace.</td></tr><tr><td valign="top"><a href="#to_bin-1">to_bin/1</a></td><td>converts a namespace to the string representation.</td></tr><tr><td valign="top"><a href="#to_bin-2">to_bin/2</a></td><td>converts a namespace and class or function name to the string representation.</td></tr><tr><td valign="top"><a href="#translate-3">translate/3</a></td><td>translate a namespace into another based on match one.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="find-2"></a>

### find/2 ###

<pre><code>
find(NS::<a href="#type-namespace">namespace()</a>, NSList::[{<a href="#type-namespace">namespace()</a>, <a href="#type-namespace">namespace()</a>}]) -&gt; {<a href="#type-namespace">namespace()</a>, <a href="#type-namespace">namespace()</a>} | false
</code></pre>
<br />

find a matching namespace based on the initial match.

<a name="join-2"></a>

### join/2 ###

<pre><code>
join(BaseNS::<a href="#type-namespace">namespace()</a>, RelativeNS::<a href="#type-namespace">namespace()</a>) -&gt; <a href="#type-namespace">namespace()</a>
</code></pre>
<br />

join two namespaces to generate only one, checking for absolute and relative.

<a name="normalize-1"></a>

### normalize/1 ###

<pre><code>
normalize(NS::<a href="#type-namespace">namespace()</a>) -&gt; <a href="#type-namespace">namespace()</a>
</code></pre>
<br />

normalize namespace removing initial empty (absolute) if any.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Str::binary()) -&gt; {<a href="#type-namespace">namespace()</a>, <a href="#type-class_name">class_name()</a>}
</code></pre>
<br />

converts a string into a 2-tuple: namespace and class name.

<a name="split-1"></a>

### split/1 ###

<pre><code>
split(NS::<a href="#type-namespace">namespace()</a>) -&gt; {<a href="#type-namespace">namespace()</a>, binary()}
</code></pre>
<br />

retrieve last part of the namespace.

<a name="to_bin-1"></a>

### to_bin/1 ###

<pre><code>
to_bin(NS::<a href="#type-namespace">namespace()</a>) -&gt; binary()
</code></pre>
<br />

converts a namespace to the string representation.

<a name="to_bin-2"></a>

### to_bin/2 ###

<pre><code>
to_bin(NS::<a href="#type-namespace">namespace()</a>, ClassName::<a href="#type-class_name">class_name()</a>) -&gt; binary()
</code></pre>
<br />

converts a namespace and class or function name to the string representation.

<a name="translate-3"></a>

### translate/3 ###

<pre><code>
translate(Source::<a href="#type-namespace">namespace()</a>, Match::<a href="#type-namespace">namespace()</a>, Target::<a href="#type-namespace">namespace()</a>) -&gt; <a href="#type-namespace">namespace()</a>
</code></pre>
<br />

translate a namespace into another based on match one.

