

# Module ersip_buf #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-options">options()</a> ###


<pre><code>
options() = map()
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = #state{options = <a href="#type-options">options()</a>, acc = binary() | iolist(), acclen = non_neg_integer(), queue = <a href="queue.md#type-queue">queue:queue</a>(binary()), queuelen = non_neg_integer(), eof = boolean(), pos = non_neg_integer(), crlf = any()} | #one_binary{options = any(), rest = binary(), eof = boolean(), pos = non_neg_integer(), crlf = any()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-2">add/2</a></td><td>Put raw binary to the buffer.</td></tr><tr><td valign="top"><a href="#has_eof-1">has_eof/1</a></td><td>Buffer has EOF.</td></tr><tr><td valign="top"><a href="#length-1">length/1</a></td><td>number of bytes accumulated inside the buffer.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>New buffer with specified options.</td></tr><tr><td valign="top"><a href="#new_dgram-1">new_dgram/1</a></td><td>New buffer with datagram.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Read number of bytes from the buffer.</td></tr><tr><td valign="top"><a href="#read_till_crlf-1">read_till_crlf/1</a></td><td>Reads buffer until CRLF is found.</td></tr><tr><td valign="top"><a href="#set_eof-1">set_eof/1</a></td><td>Add eof to the buffer.</td></tr><tr><td valign="top"><a href="#stream_postion-1">stream_postion/1</a></td><td>Current position in the stream (number of stream bytes read
out from this buffer).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-2"></a>

### add/2 ###

<pre><code>
add(Binary::binary(), One_binary::<a href="#type-state">state()</a>) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Put raw binary to the buffer.

<a name="has_eof-1"></a>

### has_eof/1 ###

<pre><code>
has_eof(One_binary::<a href="#type-state">state()</a>) -&gt; boolean()
</code></pre>
<br />

Buffer has EOF

<a name="length-1"></a>

### length/1 ###

<pre><code>
length(One_binary::<a href="#type-state">state()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

number of bytes accumulated inside the buffer.

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Options::<a href="#type-options">options()</a>) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

New buffer with specified options.
Note options are reserved in API for future use.

<a name="new_dgram-1"></a>

### new_dgram/1 ###

<pre><code>
new_dgram(Binary::binary()) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

New buffer with datagram.

<a name="read-2"></a>

### read/2 ###

<pre><code>
read(Len::pos_integer(), One_binary::<a href="#type-state">state()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, iolist() | binary(), <a href="#type-state">state()</a>} | {more_data, <a href="#type-state">state()</a>}</code></li></ul>

Read number of bytes from the buffer.

<a name="read_till_crlf-1"></a>

### read_till_crlf/1 ###

<pre><code>
read_till_crlf(One_binary::<a href="#type-state">state()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, binary(), <a href="#type-state">state()</a>} | {more_data, <a href="#type-state">state()</a>}</code></li></ul>

Reads buffer until CRLF is found.  CRLF is not included in
output and skipped on next read.

<a name="set_eof-1"></a>

### set_eof/1 ###

<pre><code>
set_eof(State::<a href="#type-state">state()</a>) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Add eof to the buffer

<a name="stream_postion-1"></a>

### stream_postion/1 ###

<pre><code>
stream_postion(One_binary::<a href="#type-state">state()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Current position in the stream (number of stream bytes read
out from this buffer).

