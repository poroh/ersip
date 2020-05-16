

# Module ersip_hdr_refer_to #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-refer_to">refer_to()</a>} | {error, term()}
</code></pre>




### <a name="type-refer_to">refer_to()</a> ###


<pre><code>
refer_to() = #refer_to{display_name = <a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>, uri = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, hparams = <a href="ersip_hparams.md#type-hparams">ersip_hparams:hparams()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_raw_params-1">all_raw_params/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#display_name-1">display_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_display_name-2">set_display_name/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_uri-2">set_uri/2</a></td><td></td></tr><tr><td valign="top"><a href="#uri-1">uri/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_raw_params-1"></a>

### all_raw_params/1 ###

<pre><code>
all_raw_params(Refer_to::<a href="#type-refer_to">refer_to()</a>) -&gt; [{binary(), binary()} | binary()]
</code></pre>
<br />

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Refer_to::<a href="#type-refer_to">refer_to()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Refer_to::<a href="#type-refer_to">refer_to()</a>) -&gt; binary()
</code></pre>
<br />

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName::binary(), Refer_to::<a href="#type-refer_to">refer_to()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

<a name="display_name-1"></a>

### display_name/1 ###

<pre><code>
display_name(Refer_to::<a href="#type-refer_to">refer_to()</a>) -&gt; <a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-refer_to">refer_to()</a>
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(URI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-refer_to">refer_to()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

<a name="set_display_name-2"></a>

### set_display_name/2 ###

<pre><code>
set_display_name(DN::<a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>, Refer_to::<a href="#type-refer_to">refer_to()</a>) -&gt; <a href="#type-refer_to">refer_to()</a>
</code></pre>
<br />

<a name="set_uri-2"></a>

### set_uri/2 ###

<pre><code>
set_uri(URI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, Refer_to::<a href="#type-refer_to">refer_to()</a>) -&gt; <a href="#type-refer_to">refer_to()</a>
</code></pre>
<br />

<a name="uri-1"></a>

### uri/1 ###

<pre><code>
uri(Refer_to::<a href="#type-refer_to">refer_to()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

