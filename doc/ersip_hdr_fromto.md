

# Module ersip_hdr_fromto #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-fromto">fromto()</a> ###


<pre><code>
fromto() = #fromto{display_name = <a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>, uri = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, hparams = <a href="ersip_hparams.md#type-hparams">ersip_hparams:hparams()</a>}
</code></pre>




### <a name="type-fromto_params">fromto_params()</a> ###


<pre><code>
fromto_params() = #{tag =&gt; <a href="#type-tag">tag()</a>, binary() =&gt; binary()}
</code></pre>




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = {invalid_fromto, no_value} | {invalid_fromto, multiple_values} | {invalid_fromto, term()}
</code></pre>

 deprecated:



### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-fromto">fromto()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = #{uri =&gt; <a href="ersip_uri.md#type-raw">ersip_uri:raw()</a>, params =&gt; <a href="ersip_hparams.md#type-raw">ersip_hparams:raw()</a>, display_name =&gt; <a href="ersip_display_name.md#type-raw">ersip_display_name:raw()</a>, tag =&gt; binary(), tag_key =&gt; binary()}
</code></pre>




### <a name="type-tag">tag()</a> ###


<pre><code>
tag() = {tag, binary()}
</code></pre>




### <a name="type-tag_key">tag_key()</a> ###


<pre><code>
tag_key() = {tag_key, binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Serialize header or tag to iolist().</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Serialize header to binary().</td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td>Build SIP raw header.</td></tr><tr><td valign="top"><a href="#display_name-1">display_name/1</a></td><td>Display name of From/To fields.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Make From/To field from binary() or raw representation.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Create empty From/To field.</td></tr><tr><td valign="top"><a href="#params-1">params/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse header from SIP raw header or from binary().</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Raw representation of SIP header.</td></tr><tr><td valign="top"><a href="#raw_params-1">raw_params/1</a></td><td>Raw params of the From/To header.</td></tr><tr><td valign="top"><a href="#set_display_name-2">set_display_name/2</a></td><td>Set display name of From/To fields.</td></tr><tr><td valign="top"><a href="#set_random_tag-2">set_random_tag/2</a></td><td>Set random tag parameter of From/To header.</td></tr><tr><td valign="top"><a href="#set_tag-2">set_tag/2</a></td><td>Set tag parameter of From/To header.</td></tr><tr><td valign="top"><a href="#set_uri-2">set_uri/2</a></td><td>Set URI of From/To fields.</td></tr><tr><td valign="top"><a href="#tag-1">tag/1</a></td><td>Tag parameter of From/To header.</td></tr><tr><td valign="top"><a href="#tag_key-1">tag_key/1</a></td><td>Erlang-comparable tag.</td></tr><tr><td valign="top"><a href="#uri-1">uri/1</a></td><td>URI of From/To fields.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; iolist()
</code></pre>
<br />

Serialize header or tag to iolist().

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; binary()
</code></pre>
<br />

Serialize header to binary().

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName::binary(), Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

Build SIP raw header.

<a name="display_name-1"></a>

### display_name/1 ###

<pre><code>
display_name(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>
</code></pre>
<br />

Display name of From/To fields.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary() | <a href="#type-raw">raw()</a>) -&gt; <a href="#type-fromto">fromto()</a>
</code></pre>
<br />

Make From/To field from binary() or raw representation.

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-fromto">fromto()</a>
</code></pre>
<br />

Create empty From/To field. (sip:127.0.0.1).

<a name="params-1"></a>

### params/1 ###

<pre><code>
params(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="#type-fromto_params">fromto_params()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

Parse header from SIP raw header or from binary().

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Raw representation of SIP header.

<a name="raw_params-1"></a>

### raw_params/1 ###

<pre><code>
raw_params(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; [{binary(), binary()} | binary()]
</code></pre>
<br />

Raw params of the From/To header.

<a name="set_display_name-2"></a>

### set_display_name/2 ###

<pre><code>
set_display_name(DN::<a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>, Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="#type-fromto">fromto()</a>
</code></pre>
<br />

Set display name of From/To fields.

<a name="set_random_tag-2"></a>

### set_random_tag/2 ###

<pre><code>
set_random_tag(NumBytes::pos_integer(), Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="#type-fromto">fromto()</a>
</code></pre>
<br />

Set random tag parameter of From/To header.
Number entropy bytes are specified by first parameter.

<a name="set_tag-2"></a>

### set_tag/2 ###

<pre><code>
set_tag(Tag::<a href="#type-tag">tag()</a> | undefined, Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="#type-fromto">fromto()</a>
</code></pre>
<br />

Set tag parameter of From/To header.
Note that set_tag(undefined, ...) is deprecated, please use clear_tag().

<a name="set_uri-2"></a>

### set_uri/2 ###

<pre><code>
set_uri(URI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="#type-fromto">fromto()</a>
</code></pre>
<br />

Set URI of From/To fields.

<a name="tag-1"></a>

### tag/1 ###

<pre><code>
tag(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; undefined | <a href="#type-tag">tag()</a>
</code></pre>
<br />

Tag parameter of From/To header.

<a name="tag_key-1"></a>

### tag_key/1 ###

<pre><code>
tag_key(Fromto::<a href="#type-tag">tag()</a> | <a href="#type-fromto">fromto()</a>) -&gt; undefined | <a href="#type-tag_key">tag_key()</a>
</code></pre>
<br />

Erlang-comparable tag. It can be used for dialog-matching for
example in ETS.

<a name="uri-1"></a>

### uri/1 ###

<pre><code>
uri(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

URI of From/To fields.

