

# Module ersip_hdr_fromto #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-fromto">fromto()</a> ###


<pre><code>
fromto() = #fromto{display_name = <a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>, uri = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, params = <a href="#type-fromto_params">fromto_params()</a>}
</code></pre>




### <a name="type-fromto_params">fromto_params()</a> ###


<pre><code>
fromto_params() = #{tag =&gt; <a href="#type-tag">tag()</a>, binary() =&gt; binary()}
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#display_name-1">display_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#params-1">params/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#raw_params-1">raw_params/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_display_name-2">set_display_name/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_random_tag-2">set_random_tag/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_tag-2">set_tag/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_uri-2">set_uri/2</a></td><td></td></tr><tr><td valign="top"><a href="#tag-1">tag/1</a></td><td></td></tr><tr><td valign="top"><a href="#tag_key-1">tag_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#uri-1">uri/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; binary()
</code></pre>
<br />

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName, Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>

<ul class="definitions"><li><code>HdrName = binary()</code></li></ul>

<a name="display_name-1"></a>

### display_name/1 ###

<pre><code>
display_name(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-fromto">fromto()</a>
</code></pre>
<br />

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-fromto">fromto()</a>
</code></pre>
<br />

<a name="params-1"></a>

### params/1 ###

<pre><code>
params(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="#type-fromto_params">fromto_params()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary()) -&gt; {ok, <a href="#type-fromto">fromto()</a>} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Error = {einval, address} | no_value | multiple_values</code></li></ul>

<a name="raw_params-1"></a>

### raw_params/1 ###

<pre><code>
raw_params(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; [{binary(), binary()} | binary()]
</code></pre>
<br />

<a name="set_display_name-2"></a>

### set_display_name/2 ###

<pre><code>
set_display_name(DN::<a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>, Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="#type-fromto">fromto()</a>
</code></pre>
<br />

<a name="set_random_tag-2"></a>

### set_random_tag/2 ###

<pre><code>
set_random_tag(NumBytes::pos_integer(), Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="#type-fromto">fromto()</a>
</code></pre>
<br />

<a name="set_tag-2"></a>

### set_tag/2 ###

<pre><code>
set_tag(Tag::<a href="#type-tag">tag()</a> | undefined, Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="#type-fromto">fromto()</a>
</code></pre>
<br />

<a name="set_uri-2"></a>

### set_uri/2 ###

<pre><code>
set_uri(URI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="#type-fromto">fromto()</a>
</code></pre>
<br />

<a name="tag-1"></a>

### tag/1 ###

<pre><code>
tag(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; undefined | <a href="#type-tag">tag()</a>
</code></pre>
<br />

<a name="tag_key-1"></a>

### tag_key/1 ###

<pre><code>
tag_key(Fromto::<a href="#type-tag">tag()</a> | <a href="#type-fromto">fromto()</a>) -&gt; undefined | <a href="#type-tag_key">tag_key()</a>
</code></pre>
<br />

<a name="uri-1"></a>

### uri/1 ###

<pre><code>
uri(Fromto::<a href="#type-fromto">fromto()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

