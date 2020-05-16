

# Module ersip_conn_se #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-bad_message">bad_message()</a> ###


<pre><code>
bad_message() = {bad_message, binary() | <a href="ersip_msg.md#type-message">ersip_msg:message()</a>, Reason::term()}
</code></pre>




### <a name="type-disconnect">disconnect()</a> ###


<pre><code>
disconnect() = {disconnect, {error, term()}}
</code></pre>




### <a name="type-new_request">new_request()</a> ###


<pre><code>
new_request() = {new_request, <a href="ersip_msg.md#type-message">ersip_msg:message()</a>}
</code></pre>




### <a name="type-new_response">new_response()</a> ###


<pre><code>
new_response() = {new_response, <a href="ersip_hdr_via.md#type-via">ersip_hdr_via:via()</a>, <a href="ersip_msg.md#type-message">ersip_msg:message()</a>}
</code></pre>




### <a name="type-side_effect">side_effect()</a> ###


<pre><code>
side_effect() = <a href="#type-bad_message">bad_message()</a> | <a href="#type-new_request">new_request()</a> | <a href="#type-new_response">new_response()</a> | <a href="#type-disconnect">disconnect()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bad_message-2">bad_message/2</a></td><td></td></tr><tr><td valign="top"><a href="#disconnect-1">disconnect/1</a></td><td></td></tr><tr><td valign="top"><a href="#new_request-1">new_request/1</a></td><td></td></tr><tr><td valign="top"><a href="#new_response-2">new_response/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bad_message-2"></a>

### bad_message/2 ###

<pre><code>
bad_message(Data::binary() | <a href="ersip_msg.md#type-message">ersip_msg:message()</a>, Reason::term()) -&gt; <a href="#type-bad_message">bad_message()</a>
</code></pre>
<br />

<a name="disconnect-1"></a>

### disconnect/1 ###

<pre><code>
disconnect(Error::{error, term()}) -&gt; <a href="#type-disconnect">disconnect()</a>
</code></pre>
<br />

<a name="new_request-1"></a>

### new_request/1 ###

<pre><code>
new_request(Message::<a href="ersip_msg.md#type-message">ersip_msg:message()</a>) -&gt; <a href="#type-new_request">new_request()</a>
</code></pre>
<br />

<a name="new_response-2"></a>

### new_response/2 ###

<pre><code>
new_response(Via::<a href="ersip_hdr_via.md#type-via">ersip_hdr_via:via()</a>, Message::<a href="ersip_msg.md#type-message">ersip_msg:message()</a>) -&gt; <a href="#type-new_response">new_response()</a>
</code></pre>
<br />

