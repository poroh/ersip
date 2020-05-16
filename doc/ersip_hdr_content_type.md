

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




### <a name="type-mime_type">mime_type()</a> ###


<pre><code>
mime_type() = {mime, Type::binary(), SubType::binary()}
</code></pre>




### <a name="type-pair">pair()</a> ###


<pre><code>
pair() = {Key::binary(), Value::binary()}
</code></pre>




### <a name="type-params">params()</a> ###


<pre><code>
params() = [<a href="#type-pair">pair()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#mime_type-1">mime_type/1</a></td><td></td></tr><tr><td valign="top"><a href="#params-1">params/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Content_type::<a href="#type-content_type">content_type()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), Content_type::<a href="#type-content_type">content_type()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary()) -&gt; <a href="#type-content_type">content_type()</a>
</code></pre>
<br />

<a name="mime_type-1"></a>

### mime_type/1 ###

<pre><code>
mime_type(Content_type::<a href="#type-content_type">content_type()</a>) -&gt; <a href="#type-mime_type">mime_type()</a>
</code></pre>
<br />

<a name="params-1"></a>

### params/1 ###

<pre><code>
params(Content_type::<a href="#type-content_type">content_type()</a>) -&gt; <a href="#type-params">params()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, <a href="#type-content_type">content_type()</a>} | {error, Error}</code></li><li><code>Error = no_content_type | {invalid_content_type, binary()}</code></li></ul>

