

# Module ephp_datetime #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_abbr_month-1">get_abbr_month/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_abbr_weekday-1">get_abbr_weekday/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_datetime-1">get_datetime/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_month-1">get_month/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_timestamp-1">get_timestamp/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_tz-2">get_tz/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_tz_time-3">get_tz_time/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_tz_version-0">get_tz_version/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_weekday-1">get_weekday/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_dst-2">is_dst/2</a></td><td></td></tr><tr><td valign="top"><a href="#posix_time-0">posix_time/0</a></td><td></td></tr><tr><td valign="top"><a href="#posix_time_ms-0">posix_time_ms/0</a></td><td></td></tr><tr><td valign="top"><a href="#set_tz-1">set_tz/1</a></td><td></td></tr><tr><td valign="top"><a href="#timestamp-0">timestamp/0</a></td><td>Show timestamp.</td></tr><tr><td valign="top"><a href="#to_bmt-1">to_bmt/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_zone-2">to_zone/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_abbr_month-1"></a>

### get_abbr_month/1 ###

<pre><code>
get_abbr_month(X1::<a href="calendar.md#type-month">calendar:month()</a>) -&gt; binary()
</code></pre>
<br />

<a name="get_abbr_weekday-1"></a>

### get_abbr_weekday/1 ###

<pre><code>
get_abbr_weekday(X1::<a href="calendar.md#type-daynum">calendar:daynum()</a>) -&gt; binary()
</code></pre>
<br />

<a name="get_datetime-1"></a>

### get_datetime/1 ###

<pre><code>
get_datetime(Timestamp::pos_integer()) -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

<a name="get_month-1"></a>

### get_month/1 ###

<pre><code>
get_month(X1::<a href="calendar.md#type-month">calendar:month()</a>) -&gt; binary()
</code></pre>
<br />

<a name="get_timestamp-1"></a>

### get_timestamp/1 ###

<pre><code>
get_timestamp(TS::integer() | float()) -&gt; <a href="timer.md#type-timestamp">timer:timestamp()</a>
</code></pre>
<br />

<a name="get_tz-2"></a>

### get_tz/2 ###

<pre><code>
get_tz(Context::<a href="#type-context">context()</a>, Line::<a href="#type-line">line()</a>) -&gt; binary()
</code></pre>
<br />

<a name="get_tz_time-3"></a>

### get_tz_time/3 ###

<pre><code>
get_tz_time(DateTime::<a href="calendar.md#type-datetime">calendar:datetime()</a>, TZ::string() | binary(), Sep::binary()) -&gt; binary()
</code></pre>
<br />

<a name="get_tz_version-0"></a>

### get_tz_version/0 ###

<pre><code>
get_tz_version() -&gt; binary()
</code></pre>
<br />

<a name="get_weekday-1"></a>

### get_weekday/1 ###

<pre><code>
get_weekday(X1::<a href="calendar.md#type-daynum">calendar:daynum()</a>) -&gt; binary()
</code></pre>
<br />

<a name="is_dst-2"></a>

### is_dst/2 ###

<pre><code>
is_dst(Datetime::<a href="calendar.md#type-datetime">calendar:datetime()</a>, RawTZ::string() | binary()) -&gt; boolean()
</code></pre>
<br />

<a name="posix_time-0"></a>

### posix_time/0 ###

<pre><code>
posix_time() -&gt; integer()
</code></pre>
<br />

<a name="posix_time_ms-0"></a>

### posix_time_ms/0 ###

<pre><code>
posix_time_ms() -&gt; integer()
</code></pre>
<br />

<a name="set_tz-1"></a>

### set_tz/1 ###

<pre><code>
set_tz(TZ::binary()) -&gt; boolean()
</code></pre>
<br />

<a name="timestamp-0"></a>

### timestamp/0 ###

<pre><code>
timestamp() -&gt; <a href="os.md#type-timestamp">os:timestamp()</a>
</code></pre>
<br />

Show timestamp. This function makes possible to overload the normal
Erlang behaviour to make tests reliable.

<a name="to_bmt-1"></a>

### to_bmt/1 ###

<pre><code>
to_bmt(DateTime::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -&gt; pos_integer()
</code></pre>
<br />

<a name="to_zone-2"></a>

### to_zone/2 ###

<pre><code>
to_zone(Date::<a href="calendar.md#type-date">calendar:date()</a>, RawTZ::string() | binary()) -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

