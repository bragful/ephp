

# Module ephp_stream_file #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_stream`](ephp_stream.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>closes a file.</td></tr><tr><td valign="top"><a href="#file_exists-1">file_exists/1</a></td><td>returns true if the URI exists, otherwise false.</td></tr><tr><td valign="top"><a href="#is_dir-1">is_dir/1</a></td><td>returns true if the URL is a directory, otherwise false.</td></tr><tr><td valign="top"><a href="#is_eof-1">is_eof/1</a></td><td>returns true if EOF is achieved by the file cursor or false otherwise.</td></tr><tr><td valign="top"><a href="#is_readable-1">is_readable/1</a></td><td>returns true if the URL is readable/accessible, otherwise false.</td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td>opens a file in the filesystem.</td></tr><tr><td valign="top"><a href="#position-2">position/2</a></td><td>moves the cursor to the specified position inside of the file.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>reads data from the file.</td></tr><tr><td valign="top"><a href="#read_file-1">read_file/1</a></td><td>read the whole file from filesystem.</td></tr><tr><td valign="top"><a href="#wildcard-1">wildcard/1</a></td><td>returns the list of URIs for a given pattern or an empty list.</td></tr><tr><td valign="top"><a href="#write-3">write/3</a></td><td>writes data to a file.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(PID::<a href="file.md#type-fd">file:fd()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

closes a file.

<a name="file_exists-1"></a>

### file_exists/1 ###

<pre><code>
file_exists(URL::<a href="ephp_stream.md#type-uri">ephp_stream:uri()</a>) -&gt; boolean()
</code></pre>
<br />

returns true if the URI exists, otherwise false.

<a name="is_dir-1"></a>

### is_dir/1 ###

<pre><code>
is_dir(URL::<a href="ephp_stream.md#type-uri">ephp_stream:uri()</a>) -&gt; boolean()
</code></pre>
<br />

returns true if the URL is a directory, otherwise false.

<a name="is_eof-1"></a>

### is_eof/1 ###

<pre><code>
is_eof(FD::<a href="file.md#type-fd">file:fd()</a>) -&gt; boolean() | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

returns true if EOF is achieved by the file cursor or false otherwise.

<a name="is_readable-1"></a>

### is_readable/1 ###

<pre><code>
is_readable(URL::<a href="ephp_stream.md#type-uri">ephp_stream:uri()</a>) -&gt; boolean()
</code></pre>
<br />

returns true if the URL is readable/accessible, otherwise false.

<a name="open-2"></a>

### open/2 ###

<pre><code>
open(URL::<a href="ephp_stream.md#type-uri">ephp_stream:uri()</a>, Options::<a href="ephp_stream.md#type-options">ephp_stream:options()</a>) -&gt; {ok, <a href="file.md#type-fd">file:fd()</a>} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

opens a file in the filesystem.

<a name="position-2"></a>

### position/2 ###

<pre><code>
position(PID::<a href="file.md#type-fd">file:fd()</a>, Location::<a href="file.md#type-location">file:location()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

moves the cursor to the specified position inside of the file.

<a name="read-2"></a>

### read/2 ###

<pre><code>
read(PID::<a href="file.md#type-fd">file:fd()</a>, Options::<a href="ephp_stream.md#type-options">ephp_stream:options()</a>) -&gt; {ok, binary()} | eof | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

reads data from the file.

<a name="read_file-1"></a>

### read_file/1 ###

<pre><code>
read_file(URL::<a href="ephp_stream.md#type-uri">ephp_stream:uri()</a>) -&gt; {ok, binary()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

read the whole file from filesystem.

<a name="wildcard-1"></a>

### wildcard/1 ###

<pre><code>
wildcard(Pattern::<a href="ephp_stream.md#type-pattern">ephp_stream:pattern()</a>) -&gt; [<a href="ephp_stream.md#type-uri">ephp_stream:uri()</a>]
</code></pre>
<br />

returns the list of URIs for a given pattern or an empty list.

<a name="write-3"></a>

### write/3 ###

<pre><code>
write(PID::<a href="file.md#type-fd">file:fd()</a>, Data::binary(), Options::<a href="ephp_stream.md#type-options">ephp_stream:options()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

writes data to a file.

