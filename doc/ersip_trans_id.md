

# Module ersip_trans_id #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-tid_client">tid_client()</a> ###


<pre><code>
tid_client() = {<a href="ersip_branch.md#type-branch_key">ersip_branch:branch_key()</a>, <a href="ersip_method.md#type-method">ersip_method:method()</a>}
</code></pre>




### <a name="type-tid_rfc2543">tid_rfc2543()</a> ###


<pre><code>
tid_rfc2543() = #tid_rfc2543{callid = <a href="ersip_hdr_callid.md#type-callid">ersip_hdr_callid:callid()</a>, ruri = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, from_tag = <a href="ersip_hdr_fromto.md#type-tag_key">ersip_hdr_fromto:tag_key()</a> | undefined, to_tag = <a href="ersip_hdr_fromto.md#type-tag_key">ersip_hdr_fromto:tag_key()</a> | undefined, cseq = <a href="ersip_hdr_cseq.md#type-cseq">ersip_hdr_cseq:cseq()</a>, topmost_via_key = <a href="ersip_hdr_via.md#type-via_key">ersip_hdr_via:via_key()</a>}
</code></pre>




### <a name="type-tid_rfc3261">tid_rfc3261()</a> ###


<pre><code>
tid_rfc3261() = #tid_rfc3261{branch_id = <a href="ersip_branch.md#type-branch_key">ersip_branch:branch_key()</a>, sent_by = <a href="ersip_hdr_via.md#type-sent_by">ersip_hdr_via:sent_by()</a>, method = <a href="ersip_method.md#type-method">ersip_method:method()</a>}
</code></pre>




### <a name="type-transaction_id">transaction_id()</a> ###


<pre><code>
transaction_id() = <a href="#type-tid_rfc3261">tid_rfc3261()</a> | <a href="#type-tid_rfc2543">tid_rfc2543()</a> | <a href="#type-tid_client">tid_client()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#make_client-2">make_client/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_server-1">make_server/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_server_cancel-1">make_server_cancel/1</a></td><td>Create transaction id of cancelled INVITE request by CANCEL SIP
message.</td></tr><tr><td valign="top"><a href="#make_server_cancel-2">make_server_cancel/2</a></td><td>Create transaction id of cancelled request by CANCEL SIP
message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="make_client-2"></a>

### make_client/2 ###

<pre><code>
make_client(Branch::<a href="ersip_branch.md#type-branch">ersip_branch:branch()</a>, Method::<a href="ersip_method.md#type-method">ersip_method:method()</a>) -&gt; <a href="#type-transaction_id">transaction_id()</a>
</code></pre>
<br />

<a name="make_server-1"></a>

### make_server/1 ###

<pre><code>
make_server(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="#type-transaction_id">transaction_id()</a>
</code></pre>
<br />

<a name="make_server_cancel-1"></a>

### make_server_cancel/1 ###

<pre><code>
make_server_cancel(CancelSipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="#type-transaction_id">transaction_id()</a>
</code></pre>
<br />

Create transaction id of cancelled INVITE request by CANCEL SIP
message.

<a name="make_server_cancel-2"></a>

### make_server_cancel/2 ###

<pre><code>
make_server_cancel(CancelSipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, Method::<a href="ersip_method.md#type-method">ersip_method:method()</a>) -&gt; <a href="#type-transaction_id">transaction_id()</a>
</code></pre>
<br />

Create transaction id of cancelled request by CANCEL SIP
message.  Method is method of request to be cancelled. Most common
is ersip_method:invite() here.

