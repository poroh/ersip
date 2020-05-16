

# Module ersip_registrar_se #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-authenticate">authenticate()</a> ###


<pre><code>
authenticate() = {authenticate, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}
</code></pre>




### <a name="type-authenticate_info">authenticate_info()</a> ###


<pre><code>
authenticate_info() = term()
</code></pre>




### <a name="type-authorize">authorize()</a> ###


<pre><code>
authorize() = {authorize, <a href="#type-authenticate_info">authenticate_info()</a>, AOR::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>}
</code></pre>




### <a name="type-binding_list">binding_list()</a> ###


<pre><code>
binding_list() = [<a href="ersip_registrar_binding.md#type-binding">ersip_registrar_binding:binding()</a>]
</code></pre>




### <a name="type-find_bindings">find_bindings()</a> ###


<pre><code>
find_bindings() = {find_bindings, AOR::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>}
</code></pre>




### <a name="type-proxy">proxy()</a> ###


<pre><code>
proxy() = {proxy, <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>}
</code></pre>

 Reply to the request and terminate request.



### <a name="type-reply">reply()</a> ###


<pre><code>
reply() = {reply, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}
</code></pre>




### <a name="type-side_effect">side_effect()</a> ###


<pre><code>
side_effect() = <a href="#type-proxy">proxy()</a> | <a href="#type-reply">reply()</a> | <a href="#type-authenticate">authenticate()</a> | <a href="#type-authorize">authorize()</a> | <a href="#type-find_bindings">find_bindings()</a> | <a href="#type-update_bindings">update_bindings()</a>
</code></pre>




### <a name="type-update_bindings">update_bindings()</a> ###


<pre><code>
update_bindings() = {update_bindings, AOR::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, <a href="#type-update_descr">update_descr()</a>}
</code></pre>




### <a name="type-update_descr">update_descr()</a> ###


<pre><code>
update_descr() = {Added::<a href="#type-binding_list">binding_list()</a>, Updated::<a href="#type-binding_list">binding_list()</a>, Removed::<a href="#type-binding_list">binding_list()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#authenticate-1">authenticate/1</a></td><td></td></tr><tr><td valign="top"><a href="#authorize-2">authorize/2</a></td><td></td></tr><tr><td valign="top"><a href="#find_bindings-1">find_bindings/1</a></td><td></td></tr><tr><td valign="top"><a href="#proxy-1">proxy/1</a></td><td></td></tr><tr><td valign="top"><a href="#reply-1">reply/1</a></td><td></td></tr><tr><td valign="top"><a href="#update_bindings-4">update_bindings/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="authenticate-1"></a>

### authenticate/1 ###

<pre><code>
authenticate(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="#type-authenticate">authenticate()</a>
</code></pre>
<br />

<a name="authorize-2"></a>

### authorize/2 ###

<pre><code>
authorize(AuthInfo::<a href="#type-authenticate_info">authenticate_info()</a>, AOR::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-authorize">authorize()</a>
</code></pre>
<br />

<a name="find_bindings-1"></a>

### find_bindings/1 ###

<pre><code>
find_bindings(AOR::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-find_bindings">find_bindings()</a>
</code></pre>
<br />

<a name="proxy-1"></a>

### proxy/1 ###

<pre><code>
proxy(RURI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-proxy">proxy()</a>
</code></pre>
<br />

<a name="reply-1"></a>

### reply/1 ###

<pre><code>
reply(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="#type-reply">reply()</a>
</code></pre>
<br />

<a name="update_bindings-4"></a>

### update_bindings/4 ###

<pre><code>
update_bindings(AOR, Added, Updated, Removed) -&gt; <a href="#type-update_bindings">update_bindings()</a>
</code></pre>

<ul class="definitions"><li><code>AOR = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a></code></li><li><code>Added = {added, <a href="#type-binding_list">binding_list()</a>}</code></li><li><code>Updated = {updated, <a href="#type-binding_list">binding_list()</a>}</code></li><li><code>Removed = {removed, <a href="#type-binding_list">binding_list()</a>}</code></li></ul>

