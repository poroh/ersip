

# Module ersip_sipmsg #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-headers">headers()</a> ###


<pre><code>
headers() = #{from =&gt; <a href="ersip_hdr_fromto.md#type-fromto">ersip_hdr_fromto:fromto()</a>, to =&gt; <a href="ersip_hdr_fromto.md#type-fromto">ersip_hdr_fromto:fromto()</a>, callid =&gt; <a href="ersip_hdr_callid.md#type-callid">ersip_hdr_callid:callid()</a>, cseq =&gt; <a href="ersip_hdr_cseq.md#type-cseq">ersip_hdr_cseq:cseq()</a>, maxforwards =&gt; <a href="ersip_hdr_maxforwards.md#type-maxforwards">ersip_hdr_maxforwards:maxforwards()</a>, topmost_via =&gt; <a href="ersip_hdr_via.md#type-via">ersip_hdr_via:via()</a>, contact =&gt; <a href="ersip_hdr_contact_list.md#type-contact_list">ersip_hdr_contact_list:contact_list()</a>}
</code></pre>




### <a name="type-known_header">known_header()</a> ###


<pre><code>
known_header() = <a href="ersip_siphdr.md#type-known_header">ersip_siphdr:known_header()</a>
</code></pre>




### <a name="type-sipmsg">sipmsg()</a> ###


<pre><code>
sipmsg() = #sipmsg{raw = <a href="ersip_msg.md#type-message">ersip_msg:message()</a>, method = <a href="ersip_method.md#type-method">ersip_method:method()</a>, ruri = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a> | undefined, headers = <a href="#type-headers">headers()</a>, user = term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#body-1">body/1</a></td><td></td></tr><tr><td valign="top"><a href="#body_bin-1">body_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#callid-1">callid/1</a></td><td></td></tr><tr><td valign="top"><a href="#clear_user_data-1">clear_user_data/1</a></td><td></td></tr><tr><td valign="top"><a href="#copy-3">copy/3</a></td><td></td></tr><tr><td valign="top"><a href="#copy_list-3">copy_list/3</a></td><td></td></tr><tr><td valign="top"><a href="#cseq-1">cseq/1</a></td><td></td></tr><tr><td valign="top"><a href="#dialog_id-2">dialog_id/2</a></td><td></td></tr><tr><td valign="top"><a href="#filter_out_parsed-2">filter_out_parsed/2</a></td><td></td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td></td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#has_body-1">has_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#header_keys-1">header_keys/1</a></td><td></td></tr><tr><td valign="top"><a href="#headers-1">headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#maxforwards-1">maxforwards/1</a></td><td></td></tr><tr><td valign="top"><a href="#method-1">method/1</a></td><td></td></tr><tr><td valign="top"><a href="#method_bin-1">method_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#new_request-2">new_request/2</a></td><td></td></tr><tr><td valign="top"><a href="#new_response-2">new_response/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>Parse Raw message and transform it to SIP message or parse
additional headers of SIP message.</td></tr><tr><td valign="top"><a href="#rack-1">rack/1</a></td><td></td></tr><tr><td valign="top"><a href="#raw_header-2">raw_header/2</a></td><td></td></tr><tr><td valign="top"><a href="#raw_message-1">raw_message/1</a></td><td></td></tr><tr><td valign="top"><a href="#reason-1">reason/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td></td></tr><tr><td valign="top"><a href="#remove_body-1">remove_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove_list-2">remove_list/2</a></td><td></td></tr><tr><td valign="top"><a href="#reply-2">reply/2</a></td><td></td></tr><tr><td valign="top"><a href="#rseq-1">rseq/1</a></td><td></td></tr><tr><td valign="top"><a href="#ruri-1">ruri/1</a></td><td></td></tr><tr><td valign="top"><a href="#serialize-1">serialize/1</a></td><td></td></tr><tr><td valign="top"><a href="#serialize_bin-1">serialize_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_body-2">set_body/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_headers-2">set_headers/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_method-2">set_method/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_raw_header-2">set_raw_header/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_raw_message-2">set_raw_message/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_reason-2">set_reason/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_ruri-2">set_ruri/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_status-2">set_status/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_user_data-2">set_user_data/2</a></td><td></td></tr><tr><td valign="top"><a href="#source-1">source/1</a></td><td></td></tr><tr><td valign="top"><a href="#source_id-1">source_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#status-1">status/1</a></td><td></td></tr><tr><td valign="top"><a href="#to-1">to/1</a></td><td></td></tr><tr><td valign="top"><a href="#topmost_via-1">topmost_via/1</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr><tr><td valign="top"><a href="#user_data-1">user_data/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; binary()
</code></pre>
<br />

<a name="body-1"></a>

### body/1 ###

<pre><code>
body(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; iolist() | binary()
</code></pre>
<br />

<a name="body_bin-1"></a>

### body_bin/1 ###

<pre><code>
body_bin(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; binary()
</code></pre>
<br />

<a name="callid-1"></a>

### callid/1 ###

<pre><code>
callid(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="ersip_hdr_callid.md#type-callid">ersip_hdr_callid:callid()</a>
</code></pre>
<br />

<a name="clear_user_data-1"></a>

### clear_user_data/1 ###

<pre><code>
clear_user_data(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="copy-3"></a>

### copy/3 ###

<pre><code>
copy(HdrNameForm::<a href="ersip_hnames.md#type-name_forms">ersip_hnames:name_forms()</a>, Src::<a href="#type-sipmsg">sipmsg()</a>, Dst::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="copy_list-3"></a>

### copy_list/3 ###

<pre><code>
copy_list(HeaderList::[<a href="ersip_hnames.md#type-name_forms">ersip_hnames:name_forms()</a>], Src::<a href="#type-sipmsg">sipmsg()</a>, Dst::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>
</code></pre>
<br />

<a name="cseq-1"></a>

### cseq/1 ###

<pre><code>
cseq(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="ersip_hdr_cseq.md#type-cseq">ersip_hdr_cseq:cseq()</a>
</code></pre>
<br />

<a name="dialog_id-2"></a>

### dialog_id/2 ###

<pre><code>
dialog_id(Role::uas | uac, Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; {ok, <a href="ersip_dialog.md#type-id">ersip_dialog:id()</a>} | no_dialog
</code></pre>
<br />

<a name="filter_out_parsed-2"></a>

### filter_out_parsed/2 ###

<pre><code>
filter_out_parsed(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>, HdrNames::[<a href="#type-known_header">known_header()</a>]) -&gt; [<a href="#type-known_header">known_header()</a>]
</code></pre>
<br />

<a name="find-2"></a>

### find/2 ###

<pre><code>
find(HdrAtom::<a href="#type-known_header">known_header()</a>, Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, term()} | not_found | {error, term()}</code></li></ul>

<a name="from-1"></a>

### from/1 ###

<pre><code>
from(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="ersip_hdr_fromto.md#type-fromto">ersip_hdr_fromto:fromto()</a>
</code></pre>
<br />

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(HdrAtom::<a href="#type-known_header">known_header()</a>, Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; Value
</code></pre>

<ul class="definitions"><li><code>Value = term()</code></li></ul>

<a name="has_body-1"></a>

### has_body/1 ###

<pre><code>
has_body(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="header_keys-1"></a>

### header_keys/1 ###

<pre><code>
header_keys(Sipmsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; [<a href="ersip_hnames.md#type-header_key">ersip_hnames:header_key()</a>]
</code></pre>
<br />

<a name="headers-1"></a>

### headers/1 ###

<pre><code>
headers(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

<a name="maxforwards-1"></a>

### maxforwards/1 ###

<pre><code>
maxforwards(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="ersip_hdr_maxforwards.md#type-maxforwards">ersip_hdr_maxforwards:maxforwards()</a>
</code></pre>
<br />

<a name="method-1"></a>

### method/1 ###

<pre><code>
method(Sipmsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="ersip_method.md#type-method">ersip_method:method()</a>
</code></pre>
<br />

<a name="method_bin-1"></a>

### method_bin/1 ###

<pre><code>
method_bin(Sipmsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; binary()
</code></pre>
<br />

<a name="new_request-2"></a>

### new_request/2 ###

<pre><code>
new_request(Method::<a href="ersip_method.md#type-method">ersip_method:method()</a>, RURI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="new_response-2"></a>

### new_response/2 ###

<pre><code>
new_response(Method::<a href="ersip_method.md#type-method">ersip_method:method()</a>, Status::<a href="ersip_status.md#type-code">ersip_status:code()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="parse-2"></a>

### parse/2 ###

<pre><code>
parse(Sipmsg::<a href="ersip_msg.md#type-message">ersip_msg:message()</a> | <a href="#type-sipmsg">sipmsg()</a> | binary() | iolist(), Headers::[<a href="#type-known_header">known_header()</a>] | all | all_required) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, <a href="#type-sipmsg">sipmsg()</a>} | {error, term()}</code></li></ul>

Parse Raw message and transform it to SIP message or parse
additional headers of SIP message.

<a name="rack-1"></a>

### rack/1 ###

<pre><code>
rack(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="ersip_hdr_rack.md#type-rack">ersip_hdr_rack:rack()</a>
</code></pre>
<br />

<a name="raw_header-2"></a>

### raw_header/2 ###

<pre><code>
raw_header(HdrName::binary(), Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

<a name="raw_message-1"></a>

### raw_message/1 ###

<pre><code>
raw_message(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="ersip_msg.md#type-message">ersip_msg:message()</a>
</code></pre>
<br />

<a name="reason-1"></a>

### reason/1 ###

<pre><code>
reason(Sipmsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; undefined | binary()
</code></pre>
<br />

<a name="remove-2"></a>

### remove/2 ###

<pre><code>
remove(HdrName::<a href="ersip_hnames.md#type-name_forms">ersip_hnames:name_forms()</a>, SipMsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="remove_body-1"></a>

### remove_body/1 ###

<pre><code>
remove_body(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="remove_list-2"></a>

### remove_list/2 ###

<pre><code>
remove_list(Rest::[<a href="ersip_hnames.md#type-name_forms">ersip_hnames:name_forms()</a>], SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>
</code></pre>
<br />

<a name="reply-2"></a>

### reply/2 ###

<pre><code>
reply(Code::<a href="ersip_reply.md#type-options">ersip_reply:options()</a> | <a href="ersip_status.md#type-code">ersip_status:code()</a>, Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="rseq-1"></a>

### rseq/1 ###

<pre><code>
rseq(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="ersip_hdr_rseq.md#type-rseq">ersip_hdr_rseq:rseq()</a>
</code></pre>
<br />

<a name="ruri-1"></a>

### ruri/1 ###

<pre><code>
ruri(Sipmsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

<a name="serialize-1"></a>

### serialize/1 ###

<pre><code>
serialize(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="serialize_bin-1"></a>

### serialize_bin/1 ###

<pre><code>
serialize_bin(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; binary()
</code></pre>
<br />

<a name="set-3"></a>

### set/3 ###

<pre><code>
set(HdrAtom::<a href="#type-known_header">known_header()</a>, Value::term(), Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; Value
</code></pre>

<ul class="definitions"><li><code>Value = term()</code></li></ul>

<a name="set_body-2"></a>

### set_body/2 ###

<pre><code>
set_body(Body::iolist() | binary(), Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="set_headers-2"></a>

### set_headers/2 ###

<pre><code>
set_headers(H::<a href="#type-headers">headers()</a>, Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="set_method-2"></a>

### set_method/2 ###

<pre><code>
set_method(MethodBin::<a href="ersip_method.md#type-method">ersip_method:method()</a> | binary(), Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="set_raw_header-2"></a>

### set_raw_header/2 ###

<pre><code>
set_raw_header(RawHdr::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>, Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; {ok, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>} | {error, term()}
</code></pre>
<br />

<a name="set_raw_message-2"></a>

### set_raw_message/2 ###

<pre><code>
set_raw_message(RawMsg::<a href="ersip_msg.md#type-message">ersip_msg:message()</a>, Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="set_reason-2"></a>

### set_reason/2 ###

<pre><code>
set_reason(Reason::binary(), Sipmsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="set_ruri-2"></a>

### set_ruri/2 ###

<pre><code>
set_ruri(URI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="set_status-2"></a>

### set_status/2 ###

<pre><code>
set_status(Code::<a href="ersip_status.md#type-code">ersip_status:code()</a>, Sipmsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>
</code></pre>
<br />

<a name="set_user_data-2"></a>

### set_user_data/2 ###

<pre><code>
set_user_data(Data::term(), Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="#type-sipmsg">sipmsg()</a>
</code></pre>
<br />

<a name="source-1"></a>

### source/1 ###

<pre><code>
source(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; undefined | <a href="ersip_source.md#type-source">ersip_source:source()</a>
</code></pre>
<br />

<a name="source_id-1"></a>

### source_id/1 ###

<pre><code>
source_id(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; term()
</code></pre>
<br />

<a name="status-1"></a>

### status/1 ###

<pre><code>
status(Sipmsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; undefined | <a href="ersip_status.md#type-code">ersip_status:code()</a>
</code></pre>
<br />

<a name="to-1"></a>

### to/1 ###

<pre><code>
to(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="ersip_hdr_fromto.md#type-fromto">ersip_hdr_fromto:fromto()</a>
</code></pre>
<br />

<a name="topmost_via-1"></a>

### topmost_via/1 ###

<pre><code>
topmost_via(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; <a href="ersip_hdr_via.md#type-via">ersip_hdr_via:via()</a>
</code></pre>
<br />

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(Sipmsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="ersip_msg.md#type-type">ersip_msg:type()</a>
</code></pre>
<br />

<a name="user_data-1"></a>

### user_data/1 ###

<pre><code>
user_data(Sipmsg::<a href="#type-sipmsg">sipmsg()</a>) -&gt; term()
</code></pre>
<br />

