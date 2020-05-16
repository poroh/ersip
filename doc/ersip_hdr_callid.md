

# Module ersip_hdr_callid #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

SIP Call-ID header.

<a name="description"></a>

## Description ##

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




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Serialize header to iolist.</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Serialize the header to binary.</td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td>Create lowlevel ersip_hdr from Call-ID header.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Create Call-ID header from binary or from raw value.</td></tr><tr><td valign="top"><a href="#make_key-1">make_key/1</a></td><td>Get value that can be used in comparision Call-ID as erlang
terms (for example using ==).</td></tr><tr><td valign="top"><a href="#make_random-1">make_random/1</a></td><td>Create random Call-Id header.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse header from binary or from ersip_hdr header.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Get raw value (in plain erlang types) of the header.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(X1::<a href="#type-callid">callid()</a>) -&gt; binary()
</code></pre>
<br />

Serialize header to iolist.

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(X1::<a href="#type-callid">callid()</a>) -&gt; binary()
</code></pre>
<br />

Serialize the header to binary.

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), CallId::<a href="#type-callid">callid()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

Create lowlevel ersip_hdr from Call-ID header.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary() | <a href="#type-raw">raw()</a>) -&gt; <a href="#type-callid">callid()</a>
</code></pre>
<br />

Create Call-ID header from binary or from raw value.
Raise error if input is not well-formed Call-ID header or incorrect raw value.
Examples:

```
    CallId = ersip_hdr_callid:make(<<"a@b">>).
    CallId2 = ersip_hdr_callid:make(<<"adwkldqwdjqklj">>).
    CallId3 = ersip_hdr_callid:make(ersip_hdr:add_value(<<"a@b">>, ersip_hdr:new(<<"CallId">>))).
```

<a name="make_key-1"></a>

### make_key/1 ###

<pre><code>
make_key(C::<a href="#type-callid">callid()</a>) -&gt; <a href="#type-callid">callid()</a>
</code></pre>
<br />

Get value that can be used in comparision Call-ID as erlang
terms (for example using ==).

<a name="make_random-1"></a>

### make_random/1 ###

<pre><code>
make_random(NumBytes::pos_integer()) -&gt; <a href="#type-callid">callid()</a>
</code></pre>
<br />

`NumBytes`: defines number bits of entropy that used in random.<br />

Create random Call-Id header.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Value::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

Parse header from binary or from ersip_hdr header.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(X1::<a href="#type-callid">callid()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Get raw value (in plain erlang types) of the header.

