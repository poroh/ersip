

# Module ersip_registrar #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-authenticate_info">authenticate_info()</a> ###


<pre><code>
authenticate_info() = term()
</code></pre>




### <a name="type-authenticate_result">authenticate_result()</a> ###


<pre><code>
authenticate_result() = {ok, {authorized, <a href="#type-authenticate_info">authenticate_info()</a>}} | {ok, {unauthorized, Reply::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}} | {error, term()}
</code></pre>




### <a name="type-authorize_result">authorize_result()</a> ###


<pre><code>
authorize_result() = {ok, authorized} | {ok, unauthorized} | {error, term()}
</code></pre>




### <a name="type-check_aor_fun">check_aor_fun()</a> ###


<pre><code>
check_aor_fun() = fun((AOR::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, RURI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; boolean())
</code></pre>




### <a name="type-config">config()</a> ###


<pre><code>
config() = #config{domains = <a href="#type-domain_set">domain_set()</a>, options = <a href="#type-options">options()</a>}
</code></pre>




### <a name="type-domain_set">domain_set()</a> ###


<pre><code>
domain_set() = any | <a href="gb_sets.md#type-set">gb_sets:set</a>(<a href="ersip_host.md#type-host">ersip_host:host()</a>)
</code></pre>




### <a name="type-exp_binding">exp_binding()</a> ###


<pre><code>
exp_binding() = {Expires::non_neg_integer(), <a href="ersip_hdr_contact.md#type-contact">ersip_hdr_contact:contact()</a>}
</code></pre>




### <a name="type-lookup_result">lookup_result()</a> ###


<pre><code>
lookup_result() = {ok, [<a href="ersip_registrar_binding.md#type-binding">ersip_registrar_binding:binding()</a>]} | {error, term()}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = #{check_aor_fun =&gt; <a href="#type-check_aor_fun">check_aor_fun()</a>, authenticate =&gt; boolean(), to_tag =&gt; auto | <a href="ersip_hdr_fromto.md#type-tag">ersip_hdr_fromto:tag()</a>, default_expires =&gt; pos_integer(), min_expires =&gt; non_neg_integer()}
</code></pre>




### <a name="type-request">request()</a> ###


<pre><code>
request() = #request{config = <a href="#type-config">config()</a>, phase = <a href="#type-request_phase">request_phase()</a>, sipmsg = <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, authinfo = undefined | <a href="#type-authenticate_info">authenticate_info()</a>, aoruri = undefined | <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, action = undefined | <a href="#type-request_action">request_action()</a>, result_bindings = [<a href="ersip_registrar_binding.md#type-binding">ersip_registrar_binding:binding()</a>]}
</code></pre>




### <a name="type-request_action">request_action()</a> ###


<pre><code>
request_action() = remove_all_bindings | {update_bindings, [<a href="#type-exp_binding">exp_binding()</a>]} | request_all_bindings
</code></pre>




### <a name="type-request_phase">request_phase()</a> ###


<pre><code>
request_phase() = check_request | authenticate | authorize | check_aor | process_contacts | process_bindings | update_bindings | prepare_answer | terminated
</code></pre>




### <a name="type-request_result">request_result()</a> ###


<pre><code>
request_result() = {<a href="#type-request">request()</a>, <a href="ersip_registrar_se.md#type-side_effect">ersip_registrar_se:side_effect()</a>}
</code></pre>




### <a name="type-update_result">update_result()</a> ###


<pre><code>
update_result() = ok | {error, term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#authenticate_result-2">authenticate_result/2</a></td><td></td></tr><tr><td valign="top"><a href="#authorize_result-2">authorize_result/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_terminated-1">is_terminated/1</a></td><td></td></tr><tr><td valign="top"><a href="#lookup_result-2">lookup_result/2</a></td><td></td></tr><tr><td valign="top"><a href="#new_config-2">new_config/2</a></td><td></td></tr><tr><td valign="top"><a href="#new_request-2">new_request/2</a></td><td></td></tr><tr><td valign="top"><a href="#update_result-2">update_result/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="authenticate_result-2"></a>

### authenticate_result/2 ###

<pre><code>
authenticate_result(Event::<a href="#type-authenticate_result">authenticate_result()</a>, Request::<a href="#type-request">request()</a>) -&gt; <a href="#type-request_result">request_result()</a>
</code></pre>
<br />

<a name="authorize_result-2"></a>

### authorize_result/2 ###

<pre><code>
authorize_result(Event::<a href="#type-authorize_result">authorize_result()</a>, Request::<a href="#type-request">request()</a>) -&gt; <a href="#type-request_result">request_result()</a>
</code></pre>
<br />

<a name="is_terminated-1"></a>

### is_terminated/1 ###

<pre><code>
is_terminated(Request::<a href="#type-request">request()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="lookup_result-2"></a>

### lookup_result/2 ###

<pre><code>
lookup_result(OkResult::<a href="#type-lookup_result">lookup_result()</a>, Request::<a href="#type-request">request()</a>) -&gt; <a href="#type-request_result">request_result()</a>
</code></pre>
<br />

<a name="new_config-2"></a>

### new_config/2 ###

<pre><code>
new_config(DomainList::[<a href="ersip_host.md#type-host">ersip_host:host()</a>] | any, Options::<a href="#type-options">options()</a>) -&gt; <a href="#type-config">config()</a>
</code></pre>
<br />

<a name="new_request-2"></a>

### new_request/2 ###

<pre><code>
new_request(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, Config::<a href="#type-config">config()</a>) -&gt; <a href="#type-request_result">request_result()</a>
</code></pre>
<br />

<a name="update_result-2"></a>

### update_result/2 ###

<pre><code>
update_result(Error::<a href="#type-update_result">update_result()</a>, Request::<a href="#type-request">request()</a>) -&gt; <a href="#type-request_result">request_result()</a>
</code></pre>
<br />

