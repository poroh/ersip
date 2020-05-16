

# Module ersip_registrar_binding #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-binding">binding()</a> ###


<pre><code>
binding() = #binding{contact = <a href="ersip_hdr_contact.md#type-contact">ersip_hdr_contact:contact()</a>, callid = <a href="ersip_hdr_callid.md#type-callid">ersip_hdr_callid:callid()</a>, cseq = <a href="ersip_hdr_cseq.md#type-cseq_num">ersip_hdr_cseq:cseq_num()</a>, expires = non_neg_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#callid_cseq-1">callid_cseq/1</a></td><td></td></tr><tr><td valign="top"><a href="#contact-1">contact/1</a></td><td></td></tr><tr><td valign="top"><a href="#contact_key-1">contact_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td></td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="callid_cseq-1"></a>

### callid_cseq/1 ###

<pre><code>
callid_cseq(Binding::<a href="#type-binding">binding()</a>) -&gt; {<a href="ersip_hdr_callid.md#type-callid">ersip_hdr_callid:callid()</a>, <a href="ersip_hdr_cseq.md#type-cseq_num">ersip_hdr_cseq:cseq_num()</a>}
</code></pre>
<br />

<a name="contact-1"></a>

### contact/1 ###

<pre><code>
contact(Binding::<a href="#type-binding">binding()</a>) -&gt; <a href="ersip_hdr_contact.md#type-contact">ersip_hdr_contact:contact()</a>
</code></pre>
<br />

<a name="contact_key-1"></a>

### contact_key/1 ###

<pre><code>
contact_key(Binding::<a href="#type-binding">binding()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

<a name="new-4"></a>

### new/4 ###

<pre><code>
new(CallId::<a href="ersip_hdr_callid.md#type-callid">ersip_hdr_callid:callid()</a>, CSeqNum::<a href="ersip_hdr_cseq.md#type-cseq_num">ersip_hdr_cseq:cseq_num()</a>, Contact::<a href="ersip_hdr_contact.md#type-contact">ersip_hdr_contact:contact()</a>, Exp::non_neg_integer()) -&gt; <a href="#type-binding">binding()</a>
</code></pre>
<br />

<a name="update-4"></a>

### update/4 ###

<pre><code>
update(NewExpiration::pos_integer(), NewCallId::<a href="ersip_hdr_callid.md#type-callid">ersip_hdr_callid:callid()</a>, NewCSeq::pos_integer(), Binding::<a href="#type-binding">binding()</a>) -&gt; <a href="#type-binding">binding()</a>
</code></pre>
<br />

