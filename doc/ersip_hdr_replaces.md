

# Module ersip_hdr_replaces #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-replaces">replaces()</a> ###


<pre><code>
replaces() = #replaces{call_id = <a href="ersip_hdr_callid.md#type-callid">ersip_hdr_callid:callid()</a>, params = <a href="ersip_hparams.md#type-hparams">ersip_hparams:hparams()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#dialog_id-1">dialog_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#early_only-1">early_only/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Replaces::<a href="#type-replaces">replaces()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Replaces::<a href="#type-replaces">replaces()</a>) -&gt; binary()
</code></pre>
<br />

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName, Replaces::<a href="#type-replaces">replaces()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>

<ul class="definitions"><li><code>HdrName = binary()</code></li></ul>

<a name="dialog_id-1"></a>

### dialog_id/1 ###

<pre><code>
dialog_id(Replaces::<a href="#type-replaces">replaces()</a>) -&gt; <a href="ersip_dialog.md#type-id">ersip_dialog:id()</a>
</code></pre>
<br />

<a name="early_only-1"></a>

### early_only/1 ###

<pre><code>
early_only(Replaces::<a href="#type-replaces">replaces()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(HeaderBin::binary()) -&gt; <a href="#type-replaces">replaces()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(HeaderBin::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary()) -&gt; {ok, <a href="#type-replaces">replaces()</a>} | {error, term()}
</code></pre>
<br />

