

# Module ersip_trans_inv_client #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-event">event()</a> ###


<pre><code>
event() = term()
</code></pre>




### <a name="type-result">result()</a> ###


<pre><code>
result() = {<a href="#type-trans_inv_client">trans_inv_client()</a>, [<a href="ersip_trans_se.md#type-effect">ersip_trans_se:effect()</a>]}
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = Calling | Proceeding | Completed | Accepted | Terminated
</code></pre>




### <a name="type-timer_type">timer_type()</a> ###


<pre><code>
timer_type() = term()
</code></pre>




### <a name="type-trans_inv_client">trans_inv_client()</a> ###


<pre><code>
trans_inv_client() = #trans_inv_client{state = <a href="#type-state">state()</a>, transport = <a href="#type-transport_type">transport_type()</a>, options = <a href="ersip.md#type-sip_options">ersip:sip_options()</a>, request = <a href="ersip_request.md#type-request">ersip_request:request()</a>, clear_reason = <a href="ersip_trans_se.md#type-clear_reason">ersip_trans_se:clear_reason()</a>, timer_a_timeout = pos_integer(), last_ack = undefined | <a href="ersip_request.md#type-request">ersip_request:request()</a>}
</code></pre>




### <a name="type-trans_inv_client_map">trans_inv_client_map()</a> ###


<pre><code>
trans_inv_client_map() = #{state =&gt; <a href="#type-state">state()</a>, transport =&gt; <a href="#type-transport_type">transport_type()</a>, options =&gt; <a href="ersip.md#type-sip_options">ersip:sip_options()</a>, request =&gt; <a href="ersip_request.md#type-request">ersip_request:request()</a>, clear_reason =&gt; <a href="ersip_trans_se.md#type-clear_reason">ersip_trans_se:clear_reason()</a>, timer_a_timeout =&gt; pos_integer(), last_ack =&gt; undefined | <a href="ersip_request.md#type-request">ersip_request:request()</a>}
</code></pre>




### <a name="type-transport_type">transport_type()</a> ###


<pre><code>
transport_type() = reliable | unreliable
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#Accepted-2">'Accepted'/2</a></td><td></td></tr><tr><td valign="top"><a href="#Calling-2">'Calling'/2</a></td><td></td></tr><tr><td valign="top"><a href="#Completed-2">'Completed'/2</a></td><td></td></tr><tr><td valign="top"><a href="#Proceeding-2">'Proceeding'/2</a></td><td></td></tr><tr><td valign="top"><a href="#Terminated-2">'Terminated'/2</a></td><td></td></tr><tr><td valign="top"><a href="#event-2">event/2</a></td><td></td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#has_final_response-1">has_final_response/1</a></td><td>Transaction has received final response.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="Accepted-2"></a>

### 'Accepted'/2 ###

<pre><code>
'Accepted'(Event::<a href="#type-event">event()</a>, Trans_inv_client::<a href="#type-trans_inv_client">trans_inv_client()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="Calling-2"></a>

### 'Calling'/2 ###

<pre><code>
'Calling'(X1::<a href="#type-event">event()</a>, Trans_inv_client::<a href="#type-trans_inv_client">trans_inv_client()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="Completed-2"></a>

### 'Completed'/2 ###

<pre><code>
'Completed'(X1::<a href="#type-event">event()</a>, Trans_inv_client::<a href="#type-trans_inv_client">trans_inv_client()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="Proceeding-2"></a>

### 'Proceeding'/2 ###

<pre><code>
'Proceeding'(X1::<a href="#type-event">event()</a>, Trans_inv_client::<a href="#type-trans_inv_client">trans_inv_client()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="Terminated-2"></a>

### 'Terminated'/2 ###

<pre><code>
'Terminated'(Event::<a href="#type-event">event()</a>, Trans_inv_client::<a href="#type-trans_inv_client">trans_inv_client()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="event-2"></a>

### event/2 ###

<pre><code>
event(Event, ServerTrans::<a href="#type-trans_inv_client">trans_inv_client()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>

<ul class="definitions"><li><code>Event = {timer, <a href="#type-timer_type">timer_type()</a>} | {send, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}</code></li></ul>

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="#type-trans_inv_client_map">trans_inv_client_map()</a>) -&gt; <a href="#type-trans_inv_client">trans_inv_client()</a>
</code></pre>
<br />

<a name="has_final_response-1"></a>

### has_final_response/1 ###

<pre><code>
has_final_response(Trans_inv_client::<a href="#type-trans_inv_client">trans_inv_client()</a>) -&gt; boolean()
</code></pre>
<br />

Transaction has received final response

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Reliable, Request, Options) -&gt; <a href="#type-result">result()</a>
</code></pre>

<ul class="definitions"><li><code>Reliable = reliable | unreliable</code></li><li><code>Request = <a href="ersip_request.md#type-request">ersip_request:request()</a></code></li><li><code>Options = <a href="ersip.md#type-sip_options">ersip:sip_options()</a></code></li></ul>

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(Trans::<a href="#type-trans_inv_client">trans_inv_client()</a>) -&gt; <a href="#type-trans_inv_client_map">trans_inv_client_map()</a>
</code></pre>
<br />

