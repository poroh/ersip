

# Module ersip_hdr_rseq #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = no_rseq | {invalid_rseq, binary()}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-rseq">rseq()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = non_neg_integer()
</code></pre>




### <a name="type-rseq">rseq()</a> ###


<pre><code>
rseq() = {rseq, non_neg_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Assemble RSeq to iolist().</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Assemble RSeq to binary().</td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td>Build raw SIP header.</td></tr><tr><td valign="top"><a href="#inc-1">inc/1</a></td><td>Increment numberic value of the rseq.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Make RSeq from binary() or raw SIP header and raw representation.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse RSeq from binary or raw SIP header representation.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Raw representation of the RSeq.</td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td>Numberic value of the rseq.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(X1::<a href="#type-rseq">rseq()</a>) -&gt; iolist()
</code></pre>
<br />

Assemble RSeq to iolist()

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(RSeq::<a href="#type-rseq">rseq()</a>) -&gt; binary()
</code></pre>
<br />

Assemble RSeq to binary()

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName::binary(), X2::<a href="#type-rseq">rseq()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

Build raw SIP header.

<a name="inc-1"></a>

### inc/1 ###

<pre><code>
inc(X1::<a href="#type-rseq">rseq()</a>) -&gt; <a href="#type-rseq">rseq()</a>
</code></pre>
<br />

Increment numberic value of the rseq.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Number::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary() | non_neg_integer()) -&gt; <a href="#type-rseq">rseq()</a>
</code></pre>
<br />

Make RSeq from binary() or raw SIP header and raw representation.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(HeaderBin::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

Parse RSeq from binary or raw SIP header representation.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(X1::<a href="#type-rseq">rseq()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Raw representation of the RSeq.

<a name="value-1"></a>

### value/1 ###

<pre><code>
value(X1::<a href="#type-rseq">rseq()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Numberic value of the rseq.

