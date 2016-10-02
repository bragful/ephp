

# Module ephp_timezone #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-abbreviation">abbreviation()</a> ###


<pre><code>
abbreviation() = binary()
</code></pre>




### <a name="type-dst">dst()</a> ###


<pre><code>
dst() = boolean()
</code></pre>




### <a name="type-offset">offset()</a> ###


<pre><code>
offset() = integer()
</code></pre>




### <a name="type-timezone">timezone()</a> ###


<pre><code>
timezone() = binary()
</code></pre>




### <a name="type-timezone_info">timezone_info()</a> ###


<pre><code>
timezone_info() = {<a href="#type-dst">dst()</a>, <a href="#type-offset">offset()</a>, <a href="#type-abbreviation">abbreviation()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#abbreviations-0">abbreviations/0</a></td><td></td></tr><tr><td valign="top"><a href="#timezone_info-1">timezone_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#timezone_to_abbreviation-2">timezone_to_abbreviation/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="abbreviations-0"></a>

### abbreviations/0 ###

<pre><code>
abbreviations() -&gt; [<a href="#type-abbreviation">abbreviation()</a>]
</code></pre>
<br />

<a name="timezone_info-1"></a>

### timezone_info/1 ###

<pre><code>
timezone_info(X1::<a href="#type-abbreviation">abbreviation()</a>) -&gt; [<a href="#type-timezone_info">timezone_info()</a>]
</code></pre>
<br />

<a name="timezone_to_abbreviation-2"></a>

### timezone_to_abbreviation/2 ###

<pre><code>
timezone_to_abbreviation(X1::<a href="#type-dst">dst()</a>, Timezone::<a href="#type-timezone">timezone()</a>) -&gt; <a href="#type-abbreviation">abbreviation()</a>
</code></pre>
<br />

