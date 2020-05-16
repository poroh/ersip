

# Module ersip_request #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-request">request()</a> ###


<pre><code>
request() = #request{sipmsg = <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, branch = <a href="ersip_branch.md#type-branch">ersip_branch:branch()</a>, nexthop = undefined | <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#branch-1">branch/1</a></td><td></td></tr><tr><td valign="top"><a href="#dialog_id-1">dialog_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#new_stateless_proxy-2">new_stateless_proxy/2</a></td><td>generate stateless proxy output request.</td></tr><tr><td valign="top"><a href="#nexthop-1">nexthop/1</a></td><td></td></tr><tr><td valign="top"><a href="#send_via_conn-2">send_via_conn/2</a></td><td>send request via SIP connection.</td></tr><tr><td valign="top"><a href="#set_nexthop-2">set_nexthop/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_sipmsg-2">set_sipmsg/2</a></td><td></td></tr><tr><td valign="top"><a href="#sipmsg-1">sipmsg/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="branch-1"></a>

### branch/1 ###

<pre><code>
branch(Request::<a href="#type-request">request()</a>) -&gt; <a href="ersip_branch.md#type-branch">ersip_branch:branch()</a>
</code></pre>
<br />

<a name="dialog_id-1"></a>

### dialog_id/1 ###

<pre><code>
dialog_id(Request::<a href="#type-request">request()</a>) -&gt; {ok, <a href="ersip_dialog.md#type-id">ersip_dialog:id()</a>} | no_dialog
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, Branch::<a href="ersip_branch.md#type-branch">ersip_branch:branch()</a>) -&gt; <a href="#type-request">request()</a>
</code></pre>
<br />

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, Branch::<a href="ersip_branch.md#type-branch">ersip_branch:branch()</a>, Nexthop::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-request">request()</a>
</code></pre>
<br />

<a name="new_stateless_proxy-2"></a>

### new_stateless_proxy/2 ###

<pre><code>
new_stateless_proxy(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, Nexthop::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-request">request()</a>
</code></pre>
<br />

generate stateless proxy output request.

<a name="nexthop-1"></a>

### nexthop/1 ###

<pre><code>
nexthop(Request::<a href="#type-request">request()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

<a name="send_via_conn-2"></a>

### send_via_conn/2 ###

<pre><code>
send_via_conn(Request::<a href="#type-request">request()</a>, SIPConn::<a href="ersip_conn.md#type-sip_conn">ersip_conn:sip_conn()</a>) -&gt; iolist()
</code></pre>
<br />

send request via SIP connection.

<a name="set_nexthop-2"></a>

### set_nexthop/2 ###

<pre><code>
set_nexthop(NexthopURI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, Request::<a href="#type-request">request()</a>) -&gt; <a href="#type-request">request()</a>
</code></pre>
<br />

<a name="set_sipmsg-2"></a>

### set_sipmsg/2 ###

<pre><code>
set_sipmsg(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, Request::<a href="#type-request">request()</a>) -&gt; <a href="#type-request">request()</a>
</code></pre>
<br />

<a name="sipmsg-1"></a>

### sipmsg/1 ###

<pre><code>
sipmsg(Request::<a href="#type-request">request()</a>) -&gt; <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>
</code></pre>
<br />

