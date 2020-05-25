

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




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = {invalid_date, bad_timezone} | {invalid_date, bad_datetime} | {invalid_date, wrong_weekday} | {invalid_date, {incorrect_date, <a href="#type-triplet">triplet()</a>}} | {invalid_date, {incorrect_time, <a href="#type-triplet">triplet()</a>}} | {invalid_date, no_datetime}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-datetime">datetime()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = {<a href="#type-date">date()</a>, <a href="#type-time">time()</a>}
</code></pre>




### <a name="type-time">time()</a> ###


<pre><code>
time() = <a href="calendar.md#type-time">calendar:time()</a>
</code></pre>




### <a name="type-triplet">triplet()</a> ###


<pre><code>
triplet() = {non_neg_integer(), non_neg_integer(), non_neg_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Serialize Date header to iolist.</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Serialize Date header to binary.</td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td>Build SIP header.</td></tr><tr><td valign="top"><a href="#date-1">date/1</a></td><td>Extract date from Date header (GMT).</td></tr><tr><td valign="top"><a href="#is_valid-1">is_valid/1</a></td><td>Check that Erlang term is valid Date header.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Make Date header from binary, raw representation of Date or
from raw SIP header.</td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td>Make date header from date and time.</td></tr><tr><td valign="top"><a href="#now-0">now/0</a></td><td>Date header for current moment of time.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse Date header from binary or from raw SIP header.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Raw representation of Date header..</td></tr><tr><td valign="top"><a href="#time-1">time/1</a></td><td>Extract time from Time header (GMT).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(DT::<a href="#type-datetime">datetime()</a>) -&gt; iolist()
</code></pre>
<br />

Serialize Date header to iolist.

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(DT::<a href="#type-datetime">datetime()</a>) -&gt; binary()
</code></pre>
<br />

Serialize Date header to binary.

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), DateTime::<a href="#type-datetime">datetime()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

Build SIP header.

<a name="date-1"></a>

### date/1 ###

<pre><code>
date(X1::<a href="#type-datetime">datetime()</a>) -&gt; <a href="#type-date">date()</a>
</code></pre>
<br />

Extract date from Date header (GMT).

<a name="is_valid-1"></a>

### is_valid/1 ###

<pre><code>
is_valid(DT::term()) -&gt; boolean()
</code></pre>
<br />

Check that Erlang term is valid Date header.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary() | <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-datetime">datetime()</a>
</code></pre>
<br />

Make Date header from binary, raw representation of Date or
from raw SIP header. If syntax is invalid then this function raises
error.

<a name="make-2"></a>

### make/2 ###

<pre><code>
make(Date::<a href="#type-date">date()</a>, Time::<a href="#type-time">time()</a>) -&gt; <a href="#type-datetime">datetime()</a>
</code></pre>
<br />

Make date header from date and time.

<a name="now-0"></a>

### now/0 ###

<pre><code>
now() -&gt; <a href="#type-datetime">datetime()</a>
</code></pre>
<br />

Date header for current moment of time.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary() | <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

Parse Date header from binary or from raw SIP header.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(X1::<a href="#type-datetime">datetime()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Raw representation of Date header..

<a name="time-1"></a>

### time/1 ###

<pre><code>
time(X1::<a href="#type-datetime">datetime()</a>) -&gt; <a href="#type-time">time()</a>
</code></pre>
<br />

Extract time from Time header (GMT).

