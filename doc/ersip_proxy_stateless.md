

# Module ersip_proxy_stateless #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-process_response_result">process_response_result()</a> ###


<pre><code>
process_response_result() = {forward, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>} | {drop, Reason::term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#branch-1">branch/1</a></td><td></td></tr><tr><td valign="top"><a href="#process_response-2">process_response/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="branch-1"></a>

### branch/1 ###

<pre><code>
branch(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="ersip_branch.md#type-branch">ersip_branch:branch()</a>
</code></pre>
<br />

<a name="process_response-2"></a>

### process_response/2 ###

<pre><code>
process_response(PrevVia, RawMsg) -&gt; <a href="#type-process_response_result">process_response_result()</a>
</code></pre>

<ul class="definitions"><li><code>PrevVia = <a href="ersip_hdr_via.md#type-via">ersip_hdr_via:via()</a></code></li><li><code>RawMsg = <a href="ersip_msg.md#type-message">ersip_msg:message()</a></code></li></ul>

