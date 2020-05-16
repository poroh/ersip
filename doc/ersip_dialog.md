

# Module ersip_dialog #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-dialog">dialog()</a> ###


<pre><code>
dialog() = #dialog{callid = <a href="ersip_hdr_callid.md#type-callid">ersip_hdr_callid:callid()</a>, local_tag = <a href="ersip_hdr_fromto.md#type-tag">ersip_hdr_fromto:tag()</a>, local_uri = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, local_seq = <a href="ersip_hdr_cseq.md#type-cseq_num">ersip_hdr_cseq:cseq_num()</a> | empty, remote_tag = <a href="ersip_hdr_fromto.md#type-tag">ersip_hdr_fromto:tag()</a> | undefined, remote_uri = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, remote_seq = <a href="ersip_hdr_cseq.md#type-cseq_num">ersip_hdr_cseq:cseq_num()</a> | empty, remote_target = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, secure = boolean(), route_set = <a href="ersip_route_set.md#type-route_set">ersip_route_set:route_set()</a>, state = <a href="#type-state">state()</a>}
</code></pre>




### <a name="type-id">id()</a> ###


<pre><code>
id() = #dialog_id{local_tag = <a href="ersip_hdr_fromto.md#type-tag_key">ersip_hdr_fromto:tag_key()</a>, remote_tag = <a href="ersip_hdr_fromto.md#type-tag_key">ersip_hdr_fromto:tag_key()</a> | undefined, callid = <a href="ersip_hdr_callid.md#type-callid">ersip_hdr_callid:callid()</a>}
</code></pre>




### <a name="type-request_type">request_type()</a> ###


<pre><code>
request_type() = target_refresh | regular
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = early | confirmed
</code></pre>




### <a name="type-uas_process_result">uas_process_result()</a> ###


<pre><code>
uas_process_result() = {ok, <a href="#type-dialog">dialog()</a>} | {reply, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}
</code></pre>




### <a name="type-uas_result">uas_result()</a> ###


<pre><code>
uas_result() = {<a href="#type-dialog">dialog()</a>, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#id-1">id/1</a></td><td>Unique identifier of the dialog.</td></tr><tr><td valign="top"><a href="#is_early-1">is_early/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_secure-1">is_secure/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_id-3">make_id/3</a></td><td></td></tr><tr><td valign="top"><a href="#remote_seq-1">remote_seq/1</a></td><td></td></tr><tr><td valign="top"><a href="#target-1">target/1</a></td><td></td></tr><tr><td valign="top"><a href="#uac_new-2">uac_new/2</a></td><td>New dialog on UAC side.</td></tr><tr><td valign="top"><a href="#uac_request-2">uac_request/2</a></td><td></td></tr><tr><td valign="top"><a href="#uac_trans_result-3">uac_trans_result/3</a></td><td></td></tr><tr><td valign="top"><a href="#uac_update-2">uac_update/2</a></td><td></td></tr><tr><td valign="top"><a href="#uas_create-2">uas_create/2</a></td><td></td></tr><tr><td valign="top"><a href="#uas_dialog_id-1">uas_dialog_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#uas_new-2">uas_new/2</a></td><td>New dialog on UAS side.</td></tr><tr><td valign="top"><a href="#uas_pass_response-3">uas_pass_response/3</a></td><td></td></tr><tr><td valign="top"><a href="#uas_process-3">uas_process/3</a></td><td></td></tr><tr><td valign="top"><a href="#uas_verify-1">uas_verify/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="id-1"></a>

### id/1 ###

<pre><code>
id(Dialog::<a href="#type-dialog">dialog()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

Unique identifier of the dialog.

<a name="is_early-1"></a>

### is_early/1 ###

<pre><code>
is_early(Dialog::<a href="#type-dialog">dialog()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="is_secure-1"></a>

### is_secure/1 ###

<pre><code>
is_secure(Dialog::<a href="#type-dialog">dialog()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="make_id-3"></a>

### make_id/3 ###

<pre><code>
make_id(LocalTagKey::<a href="ersip_hdr_fromto.md#type-tag_key">ersip_hdr_fromto:tag_key()</a>, RemoteTagKey::<a href="ersip_hdr_fromto.md#type-tag_key">ersip_hdr_fromto:tag_key()</a>, CallId::<a href="ersip_hdr_callid.md#type-callid">ersip_hdr_callid:callid()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

<a name="remote_seq-1"></a>

### remote_seq/1 ###

<pre><code>
remote_seq(Dialog::<a href="#type-dialog">dialog()</a>) -&gt; <a href="ersip_hdr_cseq.md#type-cseq_num">ersip_hdr_cseq:cseq_num()</a> | empty
</code></pre>
<br />

<a name="target-1"></a>

### target/1 ###

<pre><code>
target(Dialog::<a href="#type-dialog">dialog()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

<a name="uac_new-2"></a>

### uac_new/2 ###

<pre><code>
uac_new(Req::<a href="ersip_request.md#type-request">ersip_request:request()</a>, Response::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; {ok, <a href="#type-dialog">dialog()</a>} | {error, term()}
</code></pre>
<br />

New dialog on UAC side.

Implements 12.1.2 UAC behavior

<a name="uac_request-2"></a>

### uac_request/2 ###

<pre><code>
uac_request(Req0::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, Dialog::<a href="#type-dialog">dialog()</a>) -&gt; {<a href="#type-dialog">dialog()</a>, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}
</code></pre>
<br />

<a name="uac_trans_result-3"></a>

### uac_trans_result/3 ###

<pre><code>
uac_trans_result(Resp::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a> | timeout, ReqType::<a href="#type-request_type">request_type()</a>, Dialog::<a href="#type-dialog">dialog()</a>) -&gt; {ok, <a href="#type-dialog">dialog()</a>} | terminate_dialog
</code></pre>
<br />

<a name="uac_update-2"></a>

### uac_update/2 ###

<pre><code>
uac_update(RespSipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a> | timeout, Dialog::<a href="#type-dialog">dialog()</a>) -&gt; {ok, <a href="#type-dialog">dialog()</a>} | terminate_dialog
</code></pre>
<br />

<a name="uas_create-2"></a>

### uas_create/2 ###

<pre><code>
uas_create(Request::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, Response::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="#type-dialog">dialog()</a>
</code></pre>
<br />

<a name="uas_dialog_id-1"></a>

### uas_dialog_id/1 ###

<pre><code>
uas_dialog_id(RequestSipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; {ok, <a href="#type-id">id()</a>} | no_dialog
</code></pre>
<br />

<a name="uas_new-2"></a>

### uas_new/2 ###

<pre><code>
uas_new(Request::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, Response::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="#type-uas_result">uas_result()</a>
</code></pre>
<br />

New dialog on UAS side.

Function get request that used to create dialog and preliminary
response to this request. As result new dialog is created and
updated response is returned.

Implements 12.1.1 UAS behavior

<a name="uas_pass_response-3"></a>

### uas_pass_response/3 ###

<pre><code>
uas_pass_response(ReqSipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, RespSipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, Dialog::<a href="#type-dialog">dialog()</a>) -&gt; <a href="#type-uas_result">uas_result()</a> | terminate_dialog
</code></pre>
<br />

<a name="uas_process-3"></a>

### uas_process/3 ###

<pre><code>
uas_process(RequestSipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, ReqType::<a href="#type-request_type">request_type()</a>, Dialog::<a href="#type-dialog">dialog()</a>) -&gt; <a href="#type-uas_process_result">uas_process_result()</a>
</code></pre>
<br />

<a name="uas_verify-1"></a>

### uas_verify/1 ###

<pre><code>
uas_verify(ReqSipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; ok | {reply, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}
</code></pre>
<br />

