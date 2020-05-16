

# Module ersip_hdr_date #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-date">date()</a> ###


<pre><code>
date() = <a href="calendar.md#type-date">calendar:date()</a>
</code></pre>




### <a name="type-datetime">datetime()</a> ###


<pre><code>
datetime() = {date, <a href="calendar.md#type-datetime">calendar:datetime()</a>}
</code></pre>




### <a name="type-time">time()</a> ###


<pre><code>
time() = <a href="calendar.md#type-time">calendar:time()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#date-1">date/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_valid-1">is_valid/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_key-1">make_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#now-0">now/0</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#time-1">time/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(DateTime::<a href="#type-datetime">datetime()</a>) -&gt; [binary(), ...]
</code></pre>
<br />

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), DateTime::<a href="#type-datetime">datetime()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

<a name="date-1"></a>

### date/1 ###

<pre><code>
date(X1::<a href="#type-datetime">datetime()</a>) -&gt; <a href="#type-date">date()</a>
</code></pre>
<br />

<a name="is_valid-1"></a>

### is_valid/1 ###

<pre><code>
is_valid(DT::<a href="#type-datetime">datetime()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-datetime">datetime()</a>
</code></pre>
<br />

<a name="make-2"></a>

### make/2 ###

<pre><code>
make(Date::<a href="#type-date">date()</a>, Time::<a href="#type-time">time()</a>) -&gt; <a href="#type-datetime">datetime()</a>
</code></pre>
<br />

<a name="make_key-1"></a>

### make_key/1 ###

<pre><code>
make_key(DT::<a href="#type-datetime">datetime()</a>) -&gt; <a href="#type-datetime">datetime()</a>
</code></pre>
<br />

<a name="now-0"></a>

### now/0 ###

<pre><code>
now() -&gt; <a href="#type-datetime">datetime()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, <a href="#type-datetime">datetime()</a>} | {error, Error}</code></li><li><code>Error = bad_timezone | bad_datetime | wrong_weekday | incorrect_date | incorrect_time | no_datetime</code></li></ul>

<a name="time-1"></a>

### time/1 ###

<pre><code>
time(X1::<a href="#type-datetime">datetime()</a>) -&gt; <a href="#type-time">time()</a>
</code></pre>
<br />

