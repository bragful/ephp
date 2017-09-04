

# Module ephp_cover #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

The cover module is in charge to store the information about the lines
executed and the times.

<a name="description"></a>

## Description ##
Finally it generates a XML file with that
information in the coverage format. It uses the version 4.
<a name="types"></a>

## Data Types ##




### <a name="type-is_cover">is_cover()</a> ###


<pre><code>
is_cover() = boolean()
</code></pre>




### <a name="type-statement_type">statement_type()</a> ###


<pre><code>
statement_type() = print | eval | assign | if_block | switch | switch_case | for | foreach | while | {call, binary()} | pre_incr | post_incr | pre_decr | post_decr | {op, Type::atom()} | class | function | global | return | int | float | text | object | define | constant | switch_default
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dump-0">dump/0</a></td><td>dump the coverage information to the output file defined by
cover.output or cobertura.xml by default.</td></tr><tr><td valign="top"><a href="#get_config-0">get_config/0</a></td><td>get information about whether cover is enabled.</td></tr><tr><td valign="top"><a href="#init_file-3">init_file/3</a></td><td>store the lines of the file inside of the cover information.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>ensure the cover information is in the process.</td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td>store the information about the statement passed as a param.</td></tr><tr><td valign="top"><a href="#store-4">store/4</a></td><td>store the information about the statement passed as a param.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="dump-0"></a>

### dump/0 ###

<pre><code>
dump() -&gt; ok
</code></pre>
<br />

dump the coverage information to the output file defined by
cover.output or cobertura.xml by default.

<a name="get_config-0"></a>

### get_config/0 ###

<pre><code>
get_config() -&gt; boolean()
</code></pre>
<br />

get information about whether cover is enabled.

<a name="init_file-3"></a>

### init_file/3 ###

<pre><code>
init_file(X1::<a href="#type-is_cover">is_cover()</a>, Filename::binary(), Compiled::[<a href="#type-main_statement">main_statement()</a>]) -&gt; ok
</code></pre>
<br />

store the lines of the file inside of the cover information.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; ok
</code></pre>
<br />

ensure the cover information is in the process.

<a name="store-3"></a>

### store/3 ###

<pre><code>
store(Type::<a href="#type-statement_type">statement_type()</a>, FileOrContext::binary() | <a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a> | undefined) -&gt; ok
</code></pre>
<br />

store the information about the statement passed as a param.

<a name="store-4"></a>

### store/4 ###

<pre><code>
store(Cover::<a href="#type-is_cover">is_cover()</a>, Type::<a href="#type-statement_type">statement_type()</a>, FileOrContext::binary() | <a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a> | undefined) -&gt; ok
</code></pre>
<br />

store the information about the statement passed as a param. Calls to
store/3 only if is_cover() (first param) is true.

