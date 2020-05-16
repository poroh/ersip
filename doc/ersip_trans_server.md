

# Module ersip_trans_server #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-request">request()</a> ###


<pre><code>
request() = term()
</code></pre>




### <a name="type-response">response()</a> ###


<pre><code>
response() = term()
</code></pre>




### <a name="type-result">result()</a> ###


<pre><code>
result() = {<a href="#type-trans_server">trans_server()</a>, [<a href="ersip_trans_se.md#type-effect">ersip_trans_se:effect()</a>]}
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = Trying | Proceeding | Completed | Terminated
</code></pre>




### <a name="type-trans_server">trans_server()</a> ###


<pre><code>
trans_server() = #trans_server{state = <a href="#type-state">state()</a>, last_resp = <a href="#type-response">response()</a> | undefined, transport = reliable | unreliable, options = <a href="ersip.md#type-sip_options">ersip:sip_options()</a>}
</code></pre>




### <a name="type-trans_server_map">trans_server_map()</a> ###


<pre><code>
trans_server_map() = #{state =&gt; <a href="#type-state">state()</a>, last_resp =&gt; <a href="#type-response">response()</a> | undefined, transport =&gt; reliable | unreliable, options =&gt; <a href="ersip.md#type-sip_options">ersip:sip_options()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#event-2">event/2</a></td><td></td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#has_final_response-1">has_final_response/1</a></td><td>Transaction has sent final response.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create new ServerTrans transaction.</td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="event-2"></a>

### event/2 ###

<pre><code>
event(Event, ServerTrans::<a href="#type-trans_server">trans_server()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>

<ul class="definitions"><li><code>Event = {timer, timer_j} | {send, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>} | retransmit</code></li></ul>

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="#type-trans_server_map">trans_server_map()</a>) -&gt; <a href="#type-trans_server">trans_server()</a>
</code></pre>
<br />

<a name="has_final_response-1"></a>

### has_final_response/1 ###

<pre><code>
has_final_response(Trans_server::<a href="#type-trans_server">trans_server()</a>) -&gt; boolean()
</code></pre>
<br />

Transaction has sent final response

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Reliable, Request, Options) -&gt; <a href="#type-result">result()</a>
</code></pre>

<ul class="definitions"><li><code>Reliable = reliable | unreliable</code></li><li><code>Request = <a href="#type-request">request()</a></code></li><li><code>Options = <a href="ersip.md#type-sip_options">ersip:sip_options()</a></code></li></ul>

Create new ServerTrans transaction. Result of creation is ServerTrans state
and set of side effects that produced because of creation.

<a name="to_map-1"></a>

### to_map/1 ###

<pre><code>
to_map(Trans::<a href="#type-trans_server">trans_server()</a>) -&gt; <a href="#type-trans_server_map">trans_server_map()</a>
</code></pre>
<br />

