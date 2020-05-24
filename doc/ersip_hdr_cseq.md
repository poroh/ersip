

# Module ersip_hdr_cseq #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-cseq">cseq()</a> ###


<pre><code>
cseq() = #cseq{method = <a href="ersip_method.md#type-method">ersip_method:method()</a>, number = <a href="#type-cseq_num">cseq_num()</a>}
</code></pre>




### <a name="type-cseq_num">cseq_num()</a> ###


<pre><code>
cseq_num() = non_neg_integer()
</code></pre>




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = {invalid_cseq, binary()} | {invalid_cseq, term()} | no_cseq
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-cseq">cseq()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = {non_neg_integer(), binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Serialize to iolist.</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Serialize to binary.</td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td>Build raw SIP header from CSeq header.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Make CSeq header from binary, raw representation of CSeq of
from raw SIP header.</td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td>(<em>Deprecated</em>.) Make Cseq from parts: method and valid cseq number.</td></tr><tr><td valign="top"><a href="#make_key-1">make_key/1</a></td><td>Make erlang-comparable header data structure.</td></tr><tr><td valign="top"><a href="#method-1">method/1</a></td><td>Method of header.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Create new CSeq header from defined method.</td></tr><tr><td valign="top"><a href="#number-1">number/1</a></td><td>CSeq number from header.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse header from raw SIP header or from binary.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Raw representation in plain Erlang terms.</td></tr><tr><td valign="top"><a href="#set_method-2">set_method/2</a></td><td>Set Method of header.</td></tr><tr><td valign="top"><a href="#set_number-2">set_number/2</a></td><td>Set CSeq number of header.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Cseq::<a href="#type-cseq">cseq()</a>) -&gt; [binary(), ...]
</code></pre>
<br />

Serialize to iolist.

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Cseq::<a href="#type-cseq">cseq()</a>) -&gt; binary()
</code></pre>
<br />

Serialize to binary.

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

Build raw SIP header from CSeq header.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | <a href="#type-raw">raw()</a> | binary()) -&gt; <a href="#type-cseq">cseq()</a>
</code></pre>
<br />

Make CSeq header from binary, raw representation of CSeq of
from raw SIP header. If syntax is invalid then this function raises
error.

<a name="make-2"></a>

### make/2 ###

<pre><code>
make(Method::<a href="ersip_method.md#type-method">ersip_method:method()</a> | <a href="#type-cseq_num">cseq_num()</a>, Number::<a href="#type-cseq_num">cseq_num()</a> | <a href="ersip_method.md#type-method">ersip_method:method()</a>) -&gt; <a href="#type-cseq">cseq()</a>
</code></pre>
<br />

__This function is deprecated:__

This function has two version. make(Method, CSeq) and make(CSeq,
Method).  First one is left for backward compatibility and
deprected. Second one is more logical but we still consider it as
deprected.

Make Cseq from parts: method and valid cseq number.

<a name="make_key-1"></a>

### make_key/1 ###

<pre><code>
make_key(Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="#type-cseq">cseq()</a>
</code></pre>
<br />

Make erlang-comparable header data structure.

<a name="method-1"></a>

### method/1 ###

<pre><code>
method(Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="ersip_method.md#type-method">ersip_method:method()</a>
</code></pre>
<br />

Method of header.

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Method::<a href="ersip_method.md#type-method">ersip_method:method()</a>) -&gt; <a href="#type-cseq">cseq()</a>
</code></pre>
<br />

Create new CSeq header from defined method. CSeq number will
be set to 0.

<a name="number-1"></a>

### number/1 ###

<pre><code>
number(Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="#type-cseq_num">cseq_num()</a>
</code></pre>
<br />

CSeq number from header.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Binary::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

Parse header from raw SIP header or from binary.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Raw representation in plain Erlang terms.

<a name="set_method-2"></a>

### set_method/2 ###

<pre><code>
set_method(Method::<a href="ersip_method.md#type-method">ersip_method:method()</a>, Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="#type-cseq">cseq()</a>
</code></pre>
<br />

Set Method of header.

<a name="set_number-2"></a>

### set_number/2 ###

<pre><code>
set_number(N::<a href="#type-cseq_num">cseq_num()</a>, Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="#type-cseq">cseq()</a>
</code></pre>
<br />

Set CSeq number of header.

