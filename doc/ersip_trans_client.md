

# Module ersip_trans_client #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-clear_reason">clear_reason()</a> ###


<pre><code>
clear_reason() = <a href="ersip_trans_se.md#type-clear_reason">ersip_trans_se:clear_reason()</a>
</code></pre>




### <a name="type-request">request()</a> ###


<pre><code>
request() = term()
</code></pre>




### <a name="type-result">result()</a> ###


<pre><code>
result() = {<a href="#type-trans_client">trans_client()</a>, [<a href="ersip_trans_se.md#type-effect">ersip_trans_se:effect()</a>]}
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = Trying | Proceeding | Completed | Terminated
</code></pre>




### <a name="type-timer_type">timer_type()</a> ###


<pre><code>
timer_type() = timer_f | timer_e | timer_k
</code></pre>




### <a name="type-trans_client">trans_client()</a> ###


<pre><code>
trans_client() = #trans_client{state = <a href="#type-state">state()</a>, request = term(), options = map(), reliable_transport = reliable | unreliable, timers = #{reference() =&gt; <a href="#type-timer_type">timer_type()</a>, <a href="#type-timer_type">timer_type()</a> =&gt; reference()}, timer_e_timeout = pos_integer(), clear_reason = <a href="#type-clear_reason">clear_reason()</a> | undefined}
</code></pre>




### <a name="type-trans_client_map">trans_client_map()</a> ###


<pre><code>
trans_client_map() = #{state =&gt; <a href="#type-state">state()</a>, request =&gt; term(), options =&gt; map(), reliable_transport =&gt; reliable | unreliable, timers =&gt; #{reference() =&gt; <a href="#type-timer_type">timer_type()</a>, <a href="#type-timer_type">timer_type()</a> =&gt; reference()}, timer_e_timeout =&gt; pos_integer(), clear_reason =&gt; <a href="#type-clear_reason">clear_reason()</a> | undefined}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#clear_reason-1">clear_reason/1</a></td><td>Get transaction clear reason.</td></tr><tr><td valign="top"><a href="#event-2">event/2</a></td><td>Process event by client transaction.</td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#has_final_response-1">has_final_response/1</a></td><td>Transaction has received final response.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create new client transaction.</td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="clear_reason-1"></a>

### clear_reason/1 ###

<pre><code>
clear_reason(Trans_client::<a href="#type-trans_client">trans_client()</a>) -&gt; <a href="#type-clear_reason">clear_reason()</a>
</code></pre>
<br />

Get transaction clear reason. It is guaranteed that after
clear_trans side effect tranaction has defined clear reason.

<a name="event-2"></a>

### event/2 ###

<pre><code>
event(Event, ClientTrans::<a href="#type-trans_client">trans_client()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>

<ul class="definitions"><li><code>Event = {timer, TimerFun} | {resp, <a href="ersip_status.md#type-response_type">ersip_status:response_type()</a>, term()}</code></li><li><code>TimerFun = fun((<a href="#type-trans_client">trans_client()</a>) -&gt; <a href="#type-result">result()</a>)</code></li></ul>

Process event by client transaction.

Function retuns new client transaction state and side effects that
must be done by caller.

Defined events:
{timer, TimerFun}         - timer alarm that was requested by previous side effect
{resp, RespType, message} - response with type is recieved by client transcation.
{received, SipMsg}        - same as {resp, _, _} but more high level .

Side effects are defined in module ersip_trans_se

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="#type-trans_client_map">trans_client_map()</a>) -&gt; <a href="#type-trans_client">trans_client()</a>
</code></pre>
<br />

<a name="has_final_response-1"></a>

### has_final_response/1 ###

<pre><code>
has_final_response(Trans_client::<a href="#type-trans_client">trans_client()</a>) -&gt; boolean()
</code></pre>
<br />

Transaction has received final response

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Transport, Request, Options::<a href="ersip.md#type-sip_options">ersip:sip_options()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>

<ul class="definitions"><li><code>Transport = reliable | unreliable</code></li><li><code>Request = <a href="#type-request">request()</a></code></li></ul>

Create new client transaction. Result of creation is client
transaction state and set of side effects that produced because of
creation.

Request is not interpretted in any way.

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(Trans::<a href="#type-trans_client">trans_client()</a>) -&gt; <a href="#type-trans_client_map">trans_client_map()</a>
</code></pre>
<br />

