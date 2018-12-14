

# Module ephp_lib_file #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`ephp_func`](ephp_func.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#basename-3">basename/3</a></td><td></td></tr><tr><td valign="top"><a href="#dirname-3">dirname/3</a></td><td></td></tr><tr><td valign="top"><a href="#fclose-3">fclose/3</a></td><td></td></tr><tr><td valign="top"><a href="#feof-3">feof/3</a></td><td>returns true or false depending if EOF is achieved or not.</td></tr><tr><td valign="top"><a href="#file_exists-3">file_exists/3</a></td><td></td></tr><tr><td valign="top"><a href="#file_get_contents-3">file_get_contents/3</a></td><td></td></tr><tr><td valign="top"><a href="#fopen-4">fopen/4</a></td><td></td></tr><tr><td valign="top"><a href="#fread-4">fread/4</a></td><td></td></tr><tr><td valign="top"><a href="#fseek-5">fseek/5</a></td><td>moves the file cursor to the specified offset.</td></tr><tr><td valign="top"><a href="#fwrite-5">fwrite/5</a></td><td></td></tr><tr><td valign="top"><a href="#glob-4">glob/4</a></td><td></td></tr><tr><td valign="top"><a href="#handle_error-3">handle_error/3</a></td><td></td></tr><tr><td valign="top"><a href="#init_config-0">init_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_const-0">init_const/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#is_dir-3">is_dir/3</a></td><td></td></tr><tr><td valign="top"><a href="#is_readable-3">is_readable/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="basename-3"></a>

### basename/3 ###

<pre><code>
basename(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="dirname-3"></a>

### dirname/3 ###

<pre><code>
dirname(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="fclose-3"></a>

### fclose/3 ###

<pre><code>
fclose(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="feof-3"></a>

### feof/3 ###

<pre><code>
feof(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Resource::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

returns true or false depending if EOF is achieved or not.

<a name="file_exists-3"></a>

### file_exists/3 ###

<pre><code>
file_exists(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="file_get_contents-3"></a>

### file_get_contents/3 ###

<pre><code>
file_get_contents(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::[<a href="#type-var_value">var_value()</a>]) -&gt; false | binary()
</code></pre>
<br />

<a name="fopen-4"></a>

### fopen/4 ###

<pre><code>
fopen(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-resource">resource()</a> | false
</code></pre>
<br />

<a name="fread-4"></a>

### fread/4 ###

<pre><code>
fread(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>, X4::<a href="#type-var_value">var_value()</a>) -&gt; binary()
</code></pre>
<br />

<a name="fseek-5"></a>

### fseek/5 ###

<pre><code>
fseek(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Handle::<a href="#type-var_value">var_value()</a>, Offset::<a href="#type-var_value">var_value()</a>, Whence::<a href="#type-var_value">var_value()</a>) -&gt; 0 | -1
</code></pre>
<br />

moves the file cursor to the specified offset.

<a name="fwrite-5"></a>

### fwrite/5 ###

<pre><code>
fwrite(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Resource::<a href="#type-var_value">var_value()</a>, VData::<a href="#type-var_value">var_value()</a>, X5::<a href="#type-var_value">var_value()</a>) -&gt; non_neg_integer() | false
</code></pre>
<br />

<a name="glob-4"></a>

### glob/4 ###

<pre><code>
glob(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Pattern::<a href="#type-var_value">var_value()</a>, Flags::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="handle_error-3"></a>

### handle_error/3 ###

<pre><code>
handle_error(Error::<a href="ephp_error.md#type-error_type">ephp_error:error_type()</a>, Level::<a href="ephp_error.md#type-error_level">ephp_error:error_level()</a>, Args::term()) -&gt; string() | ignore
</code></pre>
<br />

<a name="init_config-0"></a>

### init_config/0 ###

<pre><code>
init_config() -&gt; <a href="ephp_func.md#type-php_config_results">ephp_func:php_config_results()</a>
</code></pre>
<br />

<a name="init_const-0"></a>

### init_const/0 ###

<pre><code>
init_const() -&gt; <a href="ephp_func.md#type-php_const_results">ephp_func:php_const_results()</a>
</code></pre>
<br />

<a name="init_func-0"></a>

### init_func/0 ###

<pre><code>
init_func() -&gt; <a href="ephp_func.md#type-php_function_results">ephp_func:php_function_results()</a>
</code></pre>
<br />

<a name="is_dir-3"></a>

### is_dir/3 ###

<pre><code>
is_dir(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="is_readable-3"></a>

### is_readable/3 ###

<pre><code>
is_readable(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, X3::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

