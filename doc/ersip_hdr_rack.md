

# Module ersip_hdr_rack #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = {invalid_rack, term()}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-rack">rack()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>




### <a name="type-rack">rack()</a> ###


<pre><code>
rack() = #rack{rseq = <a href="ersip_hdr_rseq.md#type-rseq">ersip_hdr_rseq:rseq()</a>, cseq = <a href="ersip_hdr_cseq.md#type-cseq">ersip_hdr_cseq:cseq()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = #{rseq =&gt; <a href="ersip_hdr_rseq.md#type-raw">ersip_hdr_rseq:raw()</a>, cseq =&gt; <a href="ersip_hdr_cseq.md#type-raw">ersip_hdr_cseq:raw()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Assemble RAck to iolist().</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Assemble RAck to binary().</td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td>Build raw SIP header.</td></tr><tr><td valign="top"><a href="#cseq-1">cseq/1</a></td><td>CSeq part of RAck.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Make RAck from binary() or raw SIP header and raw
representation.</td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td>Make RAck from RSeq and CSeq.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse RAck from binary or raw SIP header representation.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Raw representation of the RAck.</td></tr><tr><td valign="top"><a href="#rseq-1">rseq/1</a></td><td>RSeq part of RAck.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Rack::<a href="#type-rack">rack()</a>) -&gt; iolist()
</code></pre>
<br />

Assemble RAck to iolist()

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Rack::<a href="#type-rack">rack()</a>) -&gt; binary()
</code></pre>
<br />

Assemble RAck to binary()

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName::binary(), Rack::<a href="#type-rack">rack()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

Build raw SIP header.

<a name="cseq-1"></a>

### cseq/1 ###

<pre><code>
cseq(Rack::<a href="#type-rack">rack()</a>) -&gt; <a href="ersip_hdr_cseq.md#type-cseq">ersip_hdr_cseq:cseq()</a>
</code></pre>
<br />

CSeq part of RAck

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary() | <a href="#type-raw">raw()</a>) -&gt; <a href="#type-rack">rack()</a>
</code></pre>
<br />

Make RAck from binary() or raw SIP header and raw
representation.

<a name="make-2"></a>

### make/2 ###

<pre><code>
make(RSeq::<a href="ersip_hdr_rseq.md#type-rseq">ersip_hdr_rseq:rseq()</a>, CSeq::<a href="ersip_hdr_cseq.md#type-cseq">ersip_hdr_cseq:cseq()</a>) -&gt; <a href="#type-rack">rack()</a>
</code></pre>
<br />

Make RAck from RSeq and CSeq.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(HeaderBin::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

Parse RAck from binary or raw SIP header representation.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(RAck::<a href="#type-rack">rack()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Raw representation of the RAck.

<a name="rseq-1"></a>

### rseq/1 ###

<pre><code>
rseq(Rack::<a href="#type-rack">rack()</a>) -&gt; <a href="ersip_hdr_rseq.md#type-rseq">ersip_hdr_rseq:rseq()</a>
</code></pre>
<br />

RSeq part of RAck

