

# Module ersip_hdr_contact #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-contact">contact()</a> ###


<pre><code>
contact() = #contact{display_name = undefined | <a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>, uri = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, hparams = <a href="ersip_hparams.md#type-hparams">ersip_hparams:hparams()</a>}
</code></pre>




### <a name="type-contact_param">contact_param()</a> ###


<pre><code>
contact_param() = {qvalue, <a href="ersip_qvalue.md#type-qvalue">ersip_qvalue:qvalue()</a>} | {expires, <a href="#type-expires">expires()</a>} | {binary(), binary()}
</code></pre>




### <a name="type-expires">expires()</a> ###


<pre><code>
expires() = non_neg_integer()
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-contact">contact()</a>} | {error, {invalid_contact, term()}}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_raw_params-1">all_raw_params/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#display_name-1">display_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#expires-2">expires/2</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#param-2">param/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_hdr-1">parse_hdr/1</a></td><td></td></tr><tr><td valign="top"><a href="#qvalue-2">qvalue/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_expires-2">set_expires/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_param-3">set_param/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_qvalue-2">set_qvalue/2</a></td><td></td></tr><tr><td valign="top"><a href="#uri-1">uri/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_raw_params-1"></a>

### all_raw_params/1 ###

<pre><code>
all_raw_params(Contact::<a href="#type-contact">contact()</a>) -&gt; [{binary(), binary()} | binary()]
</code></pre>
<br />

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Contact::<a href="#type-contact">contact()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Contact::<a href="#type-contact">contact()</a>) -&gt; binary()
</code></pre>
<br />

<a name="display_name-1"></a>

### display_name/1 ###

<pre><code>
display_name(Contact::<a href="#type-contact">contact()</a>) -&gt; undefined | <a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>
</code></pre>
<br />

<a name="expires-2"></a>

### expires/2 ###

<pre><code>
expires(Contact::<a href="#type-contact">contact()</a>, Default::non_neg_integer()) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-contact">contact()</a>
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(URI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-contact">contact()</a>
</code></pre>
<br />

<a name="param-2"></a>

### param/2 ###

<pre><code>
param(Name::binary(), Contact::<a href="#type-contact">contact()</a>) -&gt; {ok, Value::binary()} | not_found
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

<a name="parse_hdr-1"></a>

### parse_hdr/1 ###

<pre><code>
parse_hdr(Bin::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-contact">contact()</a>)
</code></pre>
<br />

<a name="qvalue-2"></a>

### qvalue/2 ###

<pre><code>
qvalue(Contact::<a href="#type-contact">contact()</a>, Default::term()) -&gt; <a href="ersip_qvalue.md#type-qvalue">ersip_qvalue:qvalue()</a> | term()
</code></pre>
<br />

<a name="set_expires-2"></a>

### set_expires/2 ###

<pre><code>
set_expires(ExpiresVal::{expires, ExpiresVal} | ExpiresVal, Contact::<a href="#type-contact">contact()</a>) -&gt; <a href="#type-contact">contact()</a>
</code></pre>

<ul class="definitions"><li><code>ExpiresVal = non_neg_integer()</code></li></ul>

<a name="set_param-3"></a>

### set_param/3 ###

<pre><code>
set_param(Name::binary(), PValue::binary(), Contact::<a href="#type-contact">contact()</a>) -&gt; <a href="#type-contact">contact()</a>
</code></pre>
<br />

<a name="set_qvalue-2"></a>

### set_qvalue/2 ###

<pre><code>
set_qvalue(QVal::<a href="ersip_qvalue.md#type-qvalue">ersip_qvalue:qvalue()</a>, Contact::<a href="#type-contact">contact()</a>) -&gt; <a href="#type-contact">contact()</a>
</code></pre>
<br />

<a name="uri-1"></a>

### uri/1 ###

<pre><code>
uri(Contact::<a href="#type-contact">contact()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

