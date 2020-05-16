

# Module ersip_proxy_common #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-forward_options">forward_options()</a> ###


<pre><code>
forward_options() = #{nexthop =&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, routing =&gt; strict | loose}
</code></pre>




### <a name="type-forward_result">forward_result()</a> ###


<pre><code>
forward_result() = {ForwardMessage::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, ForwardOptions::<a href="#type-forward_options">forward_options()</a>}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = #{validate =&gt; <a href="#type-validate_options">validate_options()</a>, proxy =&gt; <a href="ersip_proxy.md#type-options">ersip_proxy:options()</a>}
</code></pre>




### <a name="type-proxy_params">proxy_params()</a> ###


<pre><code>
proxy_params() = <a href="ersip_proxy.md#type-options">ersip_proxy:options()</a>
</code></pre>




### <a name="type-scheme_val_fun">scheme_val_fun()</a> ###


<pre><code>
scheme_val_fun() = fun((binary() | sip) -&gt; boolean())
</code></pre>




### <a name="type-validate_options">validate_options()</a> ###


<pre><code>
validate_options() = #{to_tag =&gt; <a href="ersip_hdr_fromto.md#type-tag">ersip_hdr_fromto:tag()</a>, scheme_val_fun =&gt; <a href="#type-scheme_val_fun">scheme_val_fun()</a>, reply_on_options =&gt; boolean()}
</code></pre>




### <a name="type-validate_result">validate_result()</a> ###


<pre><code>
validate_result() = {ok, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>} | {reply, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>} | {error, term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#forward_request-3">forward_request/3</a></td><td></td></tr><tr><td valign="top"><a href="#process_route_info-2">process_route_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#request_validation-2">request_validation/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="forward_request-3"></a>

### forward_request/3 ###

<pre><code>
forward_request(Target, SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, ProxyParams::<a href="#type-proxy_params">proxy_params()</a>) -&gt; <a href="#type-forward_result">forward_result()</a>
</code></pre>

<ul class="definitions"><li><code>Target = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a></code></li></ul>

<a name="process_route_info-2"></a>

### process_route_info/2 ###

<pre><code>
process_route_info(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, ProxyParams::<a href="#type-proxy_params">proxy_params()</a>) -&gt; <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>
</code></pre>
<br />

<a name="request_validation-2"></a>

### request_validation/2 ###

<pre><code>
request_validation(RawMessage::<a href="ersip_msg.md#type-message">ersip_msg:message()</a> | <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, ValOptions::<a href="#type-options">options()</a>) -&gt; <a href="#type-validate_result">validate_result()</a>
</code></pre>
<br />

