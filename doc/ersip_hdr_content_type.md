

# Module ersip_hdr_content_type #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-content_type">content_type()</a> ###


<pre><code>
content_type() = #content_type{type = <a href="#type-mime_type">mime_type()</a>, params = <a href="#type-params">params()</a>}
</code></pre>




### <a name="type-lower_param_key">lower_param_key()</a> ###


<pre><code>
lower_param_key() = binary()
</code></pre>




### <a name="type-mime_type">mime_type()</a> ###


<pre><code>
mime_type() = {mime, Type::binary(), SubType::binary()}
</code></pre>




### <a name="type-pair">pair()</a> ###


<pre><code>
pair() = {<a href="#type-param_key">param_key()</a>, <a href="#type-param_value">param_value()</a>}
</code></pre>




### <a name="type-param_key">param_key()</a> ###


<pre><code>
param_key() = binary()
</code></pre>




### <a name="type-param_value">param_value()</a> ###


<pre><code>
param_value() = binary()
</code></pre>




### <a name="type-params">params()</a> ###


<pre><code>
params() = [<a href="#type-pair">pair()</a>]
</code></pre>




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = no_content_type | {invalid_content_type, binary()}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-content_type">content_type()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = #{type =&gt; {binary(), binary()}, params =&gt; #{<a href="#type-lower_param_key">lower_param_key()</a> =&gt; <a href="#type-param_value">param_value()</a>}}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Serialize to iolist.</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Serialize to binary.</td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td>Build raw SIP header.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Create Content-Type header from binary or from raw SIP header.</td></tr><tr><td valign="top"><a href="#mime_type-1">mime_type/1</a></td><td>MIME type pair without parameters.</td></tr><tr><td valign="top"><a href="#params-1">params/1</a></td><td>Parameters of MIME type.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse Content-Type raw SIP header.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Raw value of the Content-Type.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Content_type::<a href="#type-content_type">content_type()</a>) -&gt; iolist()
</code></pre>
<br />

Serialize to iolist.

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Content_type::<a href="#type-content_type">content_type()</a>) -&gt; binary()
</code></pre>
<br />

Serialize to binary.

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), Content_type::<a href="#type-content_type">content_type()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

Build raw SIP header.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary()) -&gt; <a href="#type-content_type">content_type()</a>
</code></pre>
<br />

Create Content-Type header from binary or from raw SIP header.

<a name="mime_type-1"></a>

### mime_type/1 ###

<pre><code>
mime_type(Content_type::<a href="#type-content_type">content_type()</a>) -&gt; <a href="#type-mime_type">mime_type()</a>
</code></pre>
<br />

MIME type pair without parameters.

<a name="params-1"></a>

### params/1 ###

<pre><code>
params(Content_type::<a href="#type-content_type">content_type()</a>) -&gt; <a href="#type-params">params()</a>
</code></pre>
<br />

Parameters of MIME type.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

Parse Content-Type raw SIP header.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Content_type::<a href="#type-content_type">content_type()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Raw value of the Content-Type.

