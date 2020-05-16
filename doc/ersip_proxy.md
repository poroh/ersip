

# Module ersip_proxy #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-check_rroute_fun">check_rroute_fun()</a> ###


<pre><code>
check_rroute_fun() = fun((<a href="ersip_hdr_route.md#type-route">ersip_hdr_route:route()</a>) -&gt; boolean())
</code></pre>




### <a name="type-context_request_map">context_request_map()</a> ###


<pre><code>
context_request_map() = #{<a href="ersip_branch.md#type-branch_key">ersip_branch:branch_key()</a> =&gt; <a href="#type-request_context">request_context()</a>}
</code></pre>




### <a name="type-internal_trans_id">internal_trans_id()</a> ###


<pre><code>
internal_trans_id() = any()
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = #{no_validate =&gt; boolean(), allow =&gt; <a href="ersip_hdr_allow.md#type-allow">ersip_hdr_allow:allow()</a>, supported =&gt; <a href="ersip_hdr_opttag_list.md#type-option_tag_list">ersip_hdr_opttag_list:option_tag_list()</a>, check_rroute_fun =&gt; <a href="#type-check_rroute_fun">check_rroute_fun()</a>, record_route_uri =&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, timer_c =&gt; non_neg_integer(), pass_503 =&gt; boolean(), cancel_timeout =&gt; non_neg_integer()}
</code></pre>




### <a name="type-params">params()</a> ###


<pre><code>
params() = <a href="ersip_proxy_common.md#type-proxy_params">ersip_proxy_common:proxy_params()</a>
</code></pre>




### <a name="type-request_context">request_context()</a> ###


<pre><code>
request_context() = #request_context{key = <a href="ersip_branch.md#type-branch_key">ersip_branch:branch_key()</a>, req = <a href="ersip_request.md#type-request">ersip_request:request()</a>, timer_c = reference() | undefined, resp = <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a> | undefined, timer_c_fired = boolean(), cancel_sent = boolean(), trans_finished = boolean(), pending_cancel = boolean()}
</code></pre>




### <a name="type-stateful">stateful()</a> ###


<pre><code>
stateful() = #stateful{phase = <a href="#type-stateful_phase">stateful_phase()</a>, options = <a href="#type-params">params()</a>, orig_sipmsg = <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, fwd_sipmsg = <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a> | undefined, req_map = <a href="#type-context_request_map">context_request_map()</a>, final_forwarded = boolean()}
</code></pre>




### <a name="type-stateful_phase">stateful_phase()</a> ###


<pre><code>
stateful_phase() = init | select_target | collect | cancelled
</code></pre>




### <a name="type-stateful_result">stateful_result()</a> ###


<pre><code>
stateful_result() = {<a href="#type-stateful">stateful()</a>, [<a href="ersip_proxy_se.md#type-side_effect">ersip_proxy_se:side_effect()</a>]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cancel-1">cancel/1</a></td><td></td></tr><tr><td valign="top"><a href="#forward_to-2">forward_to/2</a></td><td>Forward request to selected target.</td></tr><tr><td valign="top"><a href="#new_stateful-2">new_stateful/2</a></td><td>New stateful proxy request.</td></tr><tr><td valign="top"><a href="#timer_fired-2">timer_fired/2</a></td><td></td></tr><tr><td valign="top"><a href="#trans_finished-2">trans_finished/2</a></td><td></td></tr><tr><td valign="top"><a href="#trans_result-3">trans_result/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cancel-1"></a>

### cancel/1 ###

<pre><code>
cancel(Stateful::<a href="#type-stateful">stateful()</a>) -&gt; <a href="#type-stateful_result">stateful_result()</a>
</code></pre>
<br />

<a name="forward_to-2"></a>

### forward_to/2 ###

<pre><code>
forward_to(Target::Target | [Target], Stateful::<a href="#type-stateful">stateful()</a>) -&gt; <a href="#type-stateful_result">stateful_result()</a>
</code></pre>

<ul class="definitions"><li><code>Target = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a> | {<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, <a href="ersip_branch.md#type-branch">ersip_branch:branch()</a>}</code></li></ul>

Forward request to selected target. If list of URIs is
specified then all requests are forwarded simultaneously (forked).
Caller may specify branch for each target by using tuple {URI, Branch}.

If branch is not specified then it is selected randomly generated
from ?DEFAULT_BRANCH_ENTROPY random bytes.

<a name="new_stateful-2"></a>

### new_stateful/2 ###

<pre><code>
new_stateful(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, ProxyOptions::<a href="#type-options">options()</a>) -&gt; <a href="#type-stateful_result">stateful_result()</a>
</code></pre>
<br />

New stateful proxy request.

<a name="timer_fired-2"></a>

### timer_fired/2 ###

<pre><code>
timer_fired(X1::<a href="ersip_proxy_se.md#type-timer_event">ersip_proxy_se:timer_event()</a>, Stateful::<a href="#type-stateful">stateful()</a>) -&gt; <a href="#type-stateful_result">stateful_result()</a>
</code></pre>
<br />

<a name="trans_finished-2"></a>

### trans_finished/2 ###

<pre><code>
trans_finished(BranchKey::<a href="#type-internal_trans_id">internal_trans_id()</a>, Stateful::<a href="#type-stateful">stateful()</a>) -&gt; <a href="#type-stateful_result">stateful_result()</a>
</code></pre>
<br />

<a name="trans_result-3"></a>

### trans_result/3 ###

<pre><code>
trans_result(BranchKey::<a href="#type-internal_trans_id">internal_trans_id()</a>, SipMsg0::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, Stateful::<a href="#type-stateful">stateful()</a>) -&gt; <a href="#type-stateful_result">stateful_result()</a>
</code></pre>
<br />

