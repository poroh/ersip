

# Module ersip_100rel_uac #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-context">context()</a> ###


<pre><code>
context() = #context{rseq = <a href="ersip_hdr_rseq.md#type-rseq">ersip_hdr_rseq:rseq()</a>, cseq = <a href="ersip_hdr_cseq.md#type-cseq">ersip_hdr_cseq:cseq()</a>, dialog_id = <a href="ersip_dialog.md#type-id">ersip_dialog:id()</a>, pending = [<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>]}
</code></pre>




### <a name="type-id">id()</a> ###


<pre><code>
id() = {<a href="ersip_dialog.md#type-id">ersip_dialog:id()</a>, <a href="ersip_hdr_cseq.md#type-cseq">ersip_hdr_cseq:cseq()</a>}
</code></pre>




### <a name="type-maybe_context">maybe_context()</a> ###


<pre><code>
maybe_context() = <a href="#type-context">context()</a> | undefined
</code></pre>




### <a name="type-return">return()</a> ###


<pre><code>
return() = [<a href="#type-side_effect">side_effect()</a>]
</code></pre>




### <a name="type-side_effect">side_effect()</a> ###


<pre><code>
side_effect() = {save_context, <a href="#type-context">context()</a>} | {pass_reliable, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>} | {pass_unreliable, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>} | {send_prack, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>} | {stop, undefined}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#id-1">id/1</a></td><td>Returns unique identifier of the context.</td></tr><tr><td valign="top"><a href="#id_by_sipmsg-1">id_by_sipmsg/1</a></td><td></td></tr><tr><td valign="top"><a href="#require-1">require/1</a></td><td></td></tr><tr><td valign="top"><a href="#response-2">response/2</a></td><td></td></tr><tr><td valign="top"><a href="#supported-1">supported/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="id-1"></a>

### id/1 ###

<pre><code>
id(Context::<a href="#type-context">context()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

Returns unique identifier of the context.

<a name="id_by_sipmsg-1"></a>

### id_by_sipmsg/1 ###

<pre><code>
id_by_sipmsg(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="#type-id">id()</a> | undefined
</code></pre>
<br />

<a name="require-1"></a>

### require/1 ###

<pre><code>
require(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>
</code></pre>
<br />

<a name="response-2"></a>

### response/2 ###

<pre><code>
response(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, MaybeContext::<a href="#type-maybe_context">maybe_context()</a>) -&gt; <a href="#type-return">return()</a>
</code></pre>
<br />

<a name="supported-1"></a>

### supported/1 ###

<pre><code>
supported(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>
</code></pre>
<br />

