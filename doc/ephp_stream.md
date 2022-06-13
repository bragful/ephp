

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




### <a name="type-pattern">pattern()</a> ###


<pre><code>
pattern() = binary()
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>close a stream given the schema and the resource previously opened.</td></tr><tr><td valign="top"><a href="#file_exists-1">file_exists/1</a></td><td>returns true if the URI exists, otherwise false.</td></tr><tr><td valign="top"><a href="#get_initial_path-0">get_initial_path/0</a></td><td>get the initial PATH for this process.</td></tr><tr><td valign="top"><a href="#get_res_id-1">get_res_id/1</a></td><td>obtains the resource ID given a resource as param.</td></tr><tr><td valign="top"><a href="#is_dir-1">is_dir/1</a></td><td>returns true if the URL is a directory, otherwise false.</td></tr><tr><td valign="top"><a href="#is_eof-1">is_eof/1</a></td><td>returns true if EOF is achieved by the file cursor or false otherwise.</td></tr><tr><td valign="top"><a href="#is_readable-1">is_readable/1</a></td><td>returns true if the URL is readable/accessible, otherwise false.</td></tr><tr><td valign="top"><a href="#list_streams-0">list_streams/0</a></td><td>get a list of streams loaded in binary format.</td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td>open a stream given the URI and options.</td></tr><tr><td valign="top"><a href="#parse_uri-1">parse_uri/1</a></td><td>parse the URI to separate in stream and the rest of the URI.</td></tr><tr><td valign="top"><a href="#position-2">position/2</a></td><td>moves the cursor for the stream to the specified location.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>request a read to the stream implementation.</td></tr><tr><td valign="top"><a href="#read_file-1">read_file/1</a></td><td>read the whole content via URI and return it.</td></tr><tr><td valign="top"><a href="#set_initial_path-1">set_initial_path/1</a></td><td>set the initial PATH for this process.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>initialize the stream subsystem.</td></tr><tr><td valign="top"><a href="#wildcard-1">wildcard/1</a></td><td>returns the list of URIs for a given pattern or an empty list.</td></tr><tr><td valign="top"><a href="#write-3">write/3</a></td><td>request a write to the stream implementation.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Resource::<a href="#type-resource">resource()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

close a stream given the schema and the resource previously opened.

<a name="file_exists-1"></a>

### file_exists/1 ###

<pre><code>
file_exists(URL::<a href="#type-uri">uri()</a>) -&gt; boolean()
</code></pre>
<br />

returns true if the URI exists, otherwise false.

<a name="get_initial_path-0"></a>

### get_initial_path/0 ###

<pre><code>
get_initial_path() -&gt; binary()
</code></pre>
<br />

get the initial PATH for this process.

<a name="get_res_id-1"></a>

### get_res_id/1 ###

<pre><code>
get_res_id(Resource::<a href="#type-resource">resource()</a>) -&gt; integer()
</code></pre>
<br />

obtains the resource ID given a resource as param.

<a name="is_dir-1"></a>

### is_dir/1 ###

<pre><code>
is_dir(URL::<a href="#type-uri">uri()</a>) -&gt; boolean()
</code></pre>
<br />

returns true if the URL is a directory, otherwise false.

<a name="is_eof-1"></a>

### is_eof/1 ###

<pre><code>
is_eof(Resource::<a href="#type-resource">resource()</a>) -&gt; boolean() | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

returns true if EOF is achieved by the file cursor or false otherwise.

<a name="is_readable-1"></a>

### is_readable/1 ###

<pre><code>
is_readable(URL::<a href="#type-uri">uri()</a>) -&gt; boolean()
</code></pre>
<br />

returns true if the URL is readable/accessible, otherwise false.

<a name="list_streams-0"></a>

### list_streams/0 ###

<pre><code>
list_streams() -&gt; [binary()]
</code></pre>
<br />

get a list of streams loaded in binary format.

<a name="open-2"></a>

### open/2 ###

<pre><code>
open(URL::<a href="#type-uri">uri()</a>, Options::<a href="#type-options">options()</a>) -&gt; {ok, <a href="#type-resource">resource()</a>} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

open a stream given the URI and options.

<a name="parse_uri-1"></a>

### parse_uri/1 ###

<pre><code>
parse_uri(URL::binary()) -&gt; {<a href="#type-stream">stream()</a>, <a href="#type-uri">uri()</a>}
</code></pre>
<br />

parse the URI to separate in stream and the rest of the URI.

<a name="position-2"></a>

### position/2 ###

<pre><code>
position(Resource::<a href="#type-resource">resource()</a>, Location::<a href="file.md#type-location">file:location()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

moves the cursor for the stream to the specified location.

<a name="read-2"></a>

### read/2 ###

<pre><code>
read(Resource::<a href="#type-resource">resource()</a>, Options::<a href="#type-options">options()</a>) -&gt; {ok, binary()} | eof | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

request a read to the stream implementation.

<a name="read_file-1"></a>

### read_file/1 ###

<pre><code>
read_file(URL::<a href="#type-uri">uri()</a>) -&gt; {ok, binary()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

read the whole content via URI and return it.

<a name="set_initial_path-1"></a>

### set_initial_path/1 ###

<pre><code>
set_initial_path(Path::<a href="#type-uri">uri()</a>) -&gt; ok
</code></pre>
<br />

set the initial PATH for this process.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; ok
</code></pre>
<br />

initialize the stream subsystem.

<a name="wildcard-1"></a>

### wildcard/1 ###

<pre><code>
wildcard(Pattern::<a href="#type-pattern">pattern()</a>) -&gt; [<a href="#type-uri">uri()</a>]
</code></pre>
<br />

returns the list of URIs for a given pattern or an empty list.

<a name="write-3"></a>

### write/3 ###

<pre><code>
write(Resource::<a href="#type-resource">resource()</a>, Data::binary(), Options::<a href="#type-options">options()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

request a write to the stream implementation.

