

# Module ephp_stream #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-option">option()</a> ###


<pre><code>
option() = atom() | {atom(), boolean() | integer() | binary()}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>




### <a name="type-stream">stream()</a> ###


<pre><code>
stream() = binary()
</code></pre>

 file, http, ftp, ...



### <a name="type-uri">uri()</a> ###


<pre><code>
uri() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>close a stream given the schema and the resource previously opened.</td></tr><tr><td valign="top"><a href="#get_res_id-1">get_res_id/1</a></td><td>obtains the resource ID given a resource as param.</td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td>open a stream given the schema, URI and options.</td></tr><tr><td valign="top"><a href="#parse_uri-1">parse_uri/1</a></td><td>parse the URI to separate in stream and the rest of the URI.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>request a read to the stream implementation.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>initilize the stream subsystem.</td></tr><tr><td valign="top"><a href="#write-3">write/3</a></td><td>request a write to the stream implementation.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Resource::<a href="#type-resource">resource()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

close a stream given the schema and the resource previously opened.

<a name="get_res_id-1"></a>

### get_res_id/1 ###

<pre><code>
get_res_id(Resource::<a href="#type-resource">resource()</a>) -&gt; integer()
</code></pre>
<br />

obtains the resource ID given a resource as param.

<a name="open-2"></a>

### open/2 ###

<pre><code>
open(URL::<a href="#type-uri">uri()</a>, Options::<a href="#type-options">options()</a>) -&gt; {ok, <a href="#type-resource">resource()</a>} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

open a stream given the schema, URI and options.

<a name="parse_uri-1"></a>

### parse_uri/1 ###

<pre><code>
parse_uri(URL::binary()) -&gt; {<a href="#type-stream">stream()</a>, <a href="#type-uri">uri()</a>}
</code></pre>
<br />

parse the URI to separate in stream and the rest of the URI.

<a name="read-2"></a>

### read/2 ###

<pre><code>
read(Resource::<a href="#type-resource">resource()</a>, Options::<a href="#type-options">options()</a>) -&gt; {ok, binary()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

request a read to the stream implementation.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; ok
</code></pre>
<br />

initilize the stream subsystem.

<a name="write-3"></a>

### write/3 ###

<pre><code>
write(Resource::<a href="#type-resource">resource()</a>, Data::binary(), Options::<a href="#type-options">options()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

request a write to the stream implementation.

