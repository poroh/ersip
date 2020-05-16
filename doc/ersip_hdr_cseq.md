

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

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-2">make/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_key-1">make_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#method-1">method/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#number-1">number/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_method-2">set_method/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_number-2">set_number/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Cseq::<a href="#type-cseq">cseq()</a>) -&gt; [binary(), ...]
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Cseq::<a href="#type-cseq">cseq()</a>) -&gt; binary()
</code></pre>
<br />

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-cseq">cseq()</a>
</code></pre>
<br />

<a name="make-2"></a>

### make/2 ###

<pre><code>
make(Method::<a href="ersip_method.md#type-method">ersip_method:method()</a>, Number::<a href="#type-cseq_num">cseq_num()</a>) -&gt; <a href="#type-cseq">cseq()</a>
</code></pre>
<br />

<a name="make_key-1"></a>

### make_key/1 ###

<pre><code>
make_key(Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="#type-cseq">cseq()</a>
</code></pre>
<br />

<a name="method-1"></a>

### method/1 ###

<pre><code>
method(Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="ersip_method.md#type-method">ersip_method:method()</a>
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Method::<a href="ersip_method.md#type-method">ersip_method:method()</a>) -&gt; <a href="#type-cseq">cseq()</a>
</code></pre>
<br />

<a name="number-1"></a>

### number/1 ###

<pre><code>
number(Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="#type-cseq_num">cseq_num()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, <a href="#type-cseq">cseq()</a>} | {error, Error}</code></li><li><code>Error = no_cseq | multiple_cseqs | {invalid_cseq, binary()}</code></li></ul>

<a name="set_method-2"></a>

### set_method/2 ###

<pre><code>
set_method(Method::<a href="ersip_method.md#type-method">ersip_method:method()</a>, Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="#type-cseq">cseq()</a>
</code></pre>
<br />

<a name="set_number-2"></a>

### set_number/2 ###

<pre><code>
set_number(N::<a href="#type-cseq_num">cseq_num()</a>, Cseq::<a href="#type-cseq">cseq()</a>) -&gt; <a href="#type-cseq">cseq()</a>
</code></pre>
<br />

