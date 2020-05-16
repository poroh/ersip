

# Module ersip_proxy_se #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-create_trans">create_trans()</a> ###


<pre><code>
create_trans() = {create_trans, {Type::<a href="#type-trans_type">trans_type()</a>, Id::term(), <a href="ersip_request.md#type-request">ersip_request:request()</a> | <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}}
</code></pre>




### <a name="type-delete_trans">delete_trans()</a> ###


<pre><code>
delete_trans() = {delete_trans, Id::term()}
</code></pre>




### <a name="type-response">response()</a> ###


<pre><code>
response() = {response, {Id::term(), <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}}
</code></pre>




### <a name="type-select_target">select_target()</a> ###


<pre><code>
select_target() = {select_target, RURI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>}
</code></pre>




### <a name="type-set_timer">set_timer()</a> ###


<pre><code>
set_timer() = {set_timer, {timeout(), <a href="#type-timer_event">timer_event()</a>}}
</code></pre>




### <a name="type-side_effect">side_effect()</a> ###


<pre><code>
side_effect() = <a href="#type-create_trans">create_trans()</a> | <a href="#type-response">response()</a> | <a href="#type-select_target">select_target()</a> | <a href="#type-set_timer">set_timer()</a> | <a href="#type-stop">stop()</a>
</code></pre>




### <a name="type-stop">stop()</a> ###


<pre><code>
stop() = {stop, {}}
</code></pre>




### <a name="type-timer_event">timer_event()</a> ###


<pre><code>
timer_event() = {timer, <a href="ersip_branch.md#type-branch_key">ersip_branch:branch_key()</a>, reference()} | {cancel_timer, <a href="ersip_branch.md#type-branch_key">ersip_branch:branch_key()</a>}
</code></pre>




### <a name="type-trans_type">trans_type()</a> ###


<pre><code>
trans_type() = client | server
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_trans-3">create_trans/3</a></td><td>Create transaction side effect.</td></tr><tr><td valign="top"><a href="#delete_trans-1">delete_trans/1</a></td><td>Force delete transaction.</td></tr><tr><td valign="top"><a href="#response-2">response/2</a></td><td>Send response within transaction with identifier Id.</td></tr><tr><td valign="top"><a href="#select_target-1">select_target/1</a></td><td>Select target side effect.</td></tr><tr><td valign="top"><a href="#set_timer-2">set_timer/2</a></td><td>Set timer side effect.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stop side effect.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create_trans-3"></a>

### create_trans/3 ###

<pre><code>
create_trans(TransType::<a href="#type-trans_type">trans_type()</a>, Id::term(), Req::<a href="ersip_request.md#type-request">ersip_request:request()</a> | <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="#type-create_trans">create_trans()</a>
</code></pre>
<br />

Create transaction side effect.

On this event transaction of specified type need to be created.
Note ersip_trans:new_server/2 and ersip_trans:new_client/2.

Once transaction return some result then ersip_proxy:trans_result
must be called with:
1. For new SIP message: ersip_proxy:trans_result(Id, SipMsg, State)
2. For timeout: ersip_proxy:trans_result(Id, timeout, State)

<a name="delete_trans-1"></a>

### delete_trans/1 ###

<pre><code>
delete_trans(Id::term()) -&gt; <a href="#type-delete_trans">delete_trans()</a>
</code></pre>
<br />

Force delete transaction. This side effect is generated when
INVITE transaction does not respond, so timer C is fired and we
need to terminate previously created transaction to prevent
resource leak.

<a name="response-2"></a>

### response/2 ###

<pre><code>
response(Id::term(), SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="#type-response">response()</a>
</code></pre>
<br />

Send response within transaction with identifier Id.

<a name="select_target-1"></a>

### select_target/1 ###

<pre><code>
select_target(RURI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-select_target">select_target()</a>
</code></pre>
<br />

Select target side effect.

On this side effect proxy need to select destination of the
request. Once destination is selected ersip_proxy:forward_to/2 must
be called.

<a name="set_timer-2"></a>

### set_timer/2 ###

<pre><code>
set_timer(Timeout::timeout(), TimerEv::<a href="#type-timer_event">timer_event()</a>) -&gt; <a href="#type-set_timer">set_timer()</a>
</code></pre>
<br />

Set timer side effect.

On this side effect proxy need to set timer. Once this timer fired
ersip_proxy:timer_fired/2 must be called.

<a name="stop-0"></a>

### stop/0 ###

<pre><code>
stop() -&gt; <a href="#type-stop">stop()</a>
</code></pre>
<br />

Stop side effect.

State may be safely cleared after stop side effect has got.

