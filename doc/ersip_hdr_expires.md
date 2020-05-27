

# Module ersip_hdr_expires #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-expires">expires()</a> ###


<pre><code>
expires() = {expires, non_neg_integer()}
</code></pre>




### <a name="type-parse_errors">parse_errors()</a> ###


<pre><code>
parse_errors() = {invalid_expires, empty_field} | {invalid_expires, binary()}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-expires">expires()</a>} | {error, <a href="#type-parse_errors">parse_errors()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = non_neg_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Assemble header value to iolist().</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Assemble header value to binary().</td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td>Build SIP raw header.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Create Expires/Min-Expires header from raw representation or
from binary.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse Expires header.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Raw representation of the header.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(X1::<a href="#type-expires">expires()</a>) -&gt; binary()
</code></pre>
<br />

Assemble header value to iolist().
For this header it is equivalent to assemble_bin/2

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(X1::<a href="#type-expires">expires()</a>) -&gt; binary()
</code></pre>
<br />

Assemble header value to binary().

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName::binary(), V::<a href="#type-expires">expires()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

Build SIP raw header.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary() | <a href="#type-raw">raw()</a> | non_neg_integer()) -&gt; <a href="#type-expires">expires()</a>
</code></pre>
<br />

Create Expires/Min-Expires header from raw representation or
from binary.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary() | <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

Parse Expires header.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(X1::<a href="#type-expires">expires()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Raw representation of the header.

