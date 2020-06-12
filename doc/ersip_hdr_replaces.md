

# Module ersip_hdr_replaces #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = term()
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-replaces">replaces()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = #{call_id =&gt; binary(), from_tag =&gt; binary(), to_tag =&gt; binary(), early_only =&gt; boolean(), params =&gt; <a href="ersip_hparams.md#type-raw">ersip_hparams:raw()</a>}
</code></pre>




### <a name="type-replaces">replaces()</a> ###


<pre><code>
replaces() = #replaces{call_id = <a href="ersip_hdr_callid.md#type-callid">ersip_hdr_callid:callid()</a>, params = <a href="ersip_hparams.md#type-hparams">ersip_hparams:hparams()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Assemble Replaces as iolist().</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Assemble Replaces as binary().</td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td>Build raw SIP header.</td></tr><tr><td valign="top"><a href="#dialog_id-1">dialog_id/1</a></td><td>Dialog ID encoded in Replaces header.</td></tr><tr><td valign="top"><a href="#early_only-1">early_only/1</a></td><td>Early only property of Replaces header.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Create Replaces header from binary() or from raw
representation.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse Replaces header.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Raw representation of Replaces header.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Replaces::<a href="#type-replaces">replaces()</a>) -&gt; iolist()
</code></pre>
<br />

Assemble Replaces as iolist().

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Replaces::<a href="#type-replaces">replaces()</a>) -&gt; binary()
</code></pre>
<br />

Assemble Replaces as binary().

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName::binary(), Replaces::<a href="#type-replaces">replaces()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

Build raw SIP header.

<a name="dialog_id-1"></a>

### dialog_id/1 ###

<pre><code>
dialog_id(Replaces::<a href="#type-replaces">replaces()</a>) -&gt; <a href="ersip_dialog.md#type-id">ersip_dialog:id()</a>
</code></pre>
<br />

Dialog ID encoded in Replaces header.

<a name="early_only-1"></a>

### early_only/1 ###

<pre><code>
early_only(Replaces::<a href="#type-replaces">replaces()</a>) -&gt; boolean()
</code></pre>
<br />

Early only property of Replaces header.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(HeaderBin::binary()) -&gt; <a href="#type-replaces">replaces()</a>
</code></pre>
<br />

Create Replaces header from binary() or from raw
representation.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(HeaderBin::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

Parse Replaces header.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Replaces::<a href="#type-replaces">replaces()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Raw representation of Replaces header.

