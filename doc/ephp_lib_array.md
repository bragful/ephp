

# Module ephp_lib_array #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Array related functions.

__Behaviours:__ [`ephp_func`](ephp_func.md).

<a name="description"></a>

## Description ##
Here you can see documentation about the
implementation of the PHP functions for the section called _Array_.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#array_change_key_case-4">array_change_key_case/4</a></td><td></td></tr><tr><td valign="top"><a href="#array_chunk-5">array_chunk/5</a></td><td></td></tr><tr><td valign="top"><a href="#array_column-5">array_column/5</a></td><td>in an array of arrays it retries the subelements with the key passed
as a param.</td></tr><tr><td valign="top"><a href="#array_keys-3">array_keys/3</a></td><td>returns a new array with the keys.</td></tr><tr><td valign="top"><a href="#array_merge-3">array_merge/3</a></td><td></td></tr><tr><td valign="top"><a href="#array_pop-3">array_pop/3</a></td><td>returns the last element of the array and removes it from the array.</td></tr><tr><td valign="top"><a href="#array_unique-4">array_unique/4</a></td><td></td></tr><tr><td valign="top"><a href="#count-3">count/3</a></td><td></td></tr><tr><td valign="top"><a href="#current-3">current/3</a></td><td>retrieve the current element under the cursor for an array.</td></tr><tr><td valign="top"><a href="#in_array-5">in_array/5</a></td><td></td></tr><tr><td valign="top"><a href="#init_config-0">init_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#init_func-0">init_func/0</a></td><td></td></tr><tr><td valign="top"><a href="#key-3">key/3</a></td><td>returns the key under the cursor or undefined if an error happens.</td></tr><tr><td valign="top"><a href="#ksort-4">ksort/4</a></td><td>order the array based on the keys modifying the original.</td></tr><tr><td valign="top"><a href="#list-3">list/3</a></td><td></td></tr><tr><td valign="top"><a href="#next-3">next/3</a></td><td>moves the cursor to the next element and retrieves it if it's possible,
false otherwise.</td></tr><tr><td valign="top"><a href="#php_end-3">php_end/3</a></td><td>moves the array cursor to the last element and retrieves it.</td></tr><tr><td valign="top"><a href="#prev-3">prev/3</a></td><td>moves the cursor to the previous element and retrieves it if it's
possible, false otherwise.</td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td>resets the cursor for an array moving it to the first element and
retrieving that element if it's exists, false otherwise.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="array_change_key_case-4"></a>

### array_change_key_case/4 ###

<pre><code>
array_change_key_case(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>, Flags::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="array_chunk-5"></a>

### array_chunk/5 ###

<pre><code>
array_chunk(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>, Size::<a href="#type-var_value">var_value()</a>, PreserveKeys::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="array_column-5"></a>

### array_column/5 ###

<pre><code>
array_column(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>, ColKey::<a href="#type-var_value">var_value()</a>, IdxKey::<a href="#type-var_value">var_value()</a>) -&gt; false | <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

in an array of arrays it retries the subelements with the key passed
as a param. It let you to change the new key to retrieve the elements.

<a name="array_keys-3"></a>

### array_keys/3 ###

<pre><code>
array_keys(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

returns a new array with the keys.

<a name="array_merge-3"></a>

### array_merge/3 ###

<pre><code>
array_merge(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Arrays::[<a href="#type-var_value">var_value()</a>]) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="array_pop-3"></a>

### array_pop/3 ###

<pre><code>
array_pop(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

returns the last element of the array and removes it from the array.

<a name="array_unique-4"></a>

### array_unique/4 ###

<pre><code>
array_unique(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>, Flags::<a href="#type-var_value">var_value()</a>) -&gt; <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

<a name="count-3"></a>

### count/3 ###

<pre><code>
count(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>) -&gt; integer()
</code></pre>
<br />

<a name="current-3"></a>

### current/3 ###

<pre><code>
current(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>) -&gt; false | <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

retrieve the current element under the cursor for an array.

<a name="in_array-5"></a>

### in_array/5 ###

<pre><code>
in_array(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Key::<a href="#type-var_value">var_value()</a>, Array::<a href="#type-var_value">var_value()</a>, Strict::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="init_config-0"></a>

### init_config/0 ###

<pre><code>
init_config() -&gt; <a href="ephp_func.md#type-php_config_results">ephp_func:php_config_results()</a>
</code></pre>
<br />

<a name="init_func-0"></a>

### init_func/0 ###

<pre><code>
init_func() -&gt; <a href="ephp_func.md#type-php_function_results">ephp_func:php_function_results()</a>
</code></pre>
<br />

<a name="key-3"></a>

### key/3 ###

<pre><code>
key(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>) -&gt; undefined | <a href="#type-mixed">mixed()</a>
</code></pre>
<br />

returns the key under the cursor or undefined if an error happens.

<a name="ksort-4"></a>

### ksort/4 ###

<pre><code>
ksort(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>, SortType::<a href="#type-var_value">var_value()</a>) -&gt; boolean()
</code></pre>
<br />

order the array based on the keys modifying the original. The function
returns true if the ordering was ok, otherwise false. We can use different
sort types: SORT_REGULAR (default), SORT_NUMERIC, SORT_STRING,
SORT_LOCALE_STRING, SORT_NATURAL, SORT_FLAG_CASE.

<a name="list-3"></a>

### list/3 ###

<pre><code>
list(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Vars::[<a href="#type-var_value">var_value()</a>]) -&gt; <a href="#type-ephp_array">ephp_array()</a> | undefined
</code></pre>
<br />

<a name="next-3"></a>

### next/3 ###

<pre><code>
next(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>) -&gt; false | <a href="#type-mixed">mixed()</a>
</code></pre>
<br />

moves the cursor to the next element and retrieves it if it's possible,
false otherwise.

<a name="php_end-3"></a>

### php_end/3 ###

<pre><code>
php_end(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>) -&gt; false | <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

moves the array cursor to the last element and retrieves it.

<a name="prev-3"></a>

### prev/3 ###

<pre><code>
prev(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>) -&gt; false | <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

moves the cursor to the previous element and retrieves it if it's
possible, false otherwise.

<a name="reset-3"></a>

### reset/3 ###

<pre><code>
reset(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>, Array::<a href="#type-var_value">var_value()</a>) -&gt; false | <a href="#type-ephp_array">ephp_array()</a>
</code></pre>
<br />

resets the cursor for an array moving it to the first element and
retrieving that element if it's exists, false otherwise.

