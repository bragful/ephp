

# Module ephp_stream_file #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_stream`](ephp_stream.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>closes a file.</td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td>opens a file in the filesystem.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>reads data from the file.</td></tr><tr><td valign="top"><a href="#write-3">write/3</a></td><td>writes data to a file.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(File_descriptor::#file_descriptor{}) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

closes a file.

<a name="open-2"></a>

### open/2 ###

<pre><code>
open(File::<a href="ephp_stream.md#type-uri">ephp_stream:uri()</a>, Options::<a href="ephp_stream.md#type-options">ephp_stream:options()</a>) -&gt; {ok, #file_descriptor{}} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

opens a file in the filesystem.

<a name="read-2"></a>

### read/2 ###

<pre><code>
read(File_descriptor::#file_descriptor{}, Options::<a href="ephp_stream.md#type-options">ephp_stream:options()</a>) -&gt; {ok, binary()} | eof | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

reads data from the file.

<a name="write-3"></a>

### write/3 ###

<pre><code>
write(File_descriptor::#file_descriptor{}, Data::binary(), Options::<a href="ephp_stream.md#type-options">ephp_stream:options()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

writes data to a file.

