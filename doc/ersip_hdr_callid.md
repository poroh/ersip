

# Module ersip_hdr_callid #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-callid">callid()</a> ###


<pre><code>
callid() = {callid, binary()}
</code></pre>




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = no_callid | {invalid_callid, binary()}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-callid">callid()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_key-1">make_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_random-1">make_random/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(X1::<a href="#type-callid">callid()</a>) -&gt; binary()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(X1::<a href="#type-callid">callid()</a>) -&gt; binary()
</code></pre>
<br />

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), CallId::<a href="#type-callid">callid()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary()) -&gt; <a href="#type-callid">callid()</a>
</code></pre>
<br />

<a name="make_key-1"></a>

### make_key/1 ###

<pre><code>
make_key(C::<a href="#type-callid">callid()</a>) -&gt; <a href="#type-callid">callid()</a>
</code></pre>
<br />

<a name="make_random-1"></a>

### make_random/1 ###

<pre><code>
make_random(NumBytes::pos_integer()) -&gt; <a href="#type-callid">callid()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Value::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

