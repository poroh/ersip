

# Module ersip_trans_se #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-clear_reason">clear_reason()</a> ###


<pre><code>
clear_reason() = normal | timeout | no_ack
</code></pre>




### <a name="type-clear_trans">clear_trans()</a> ###


<pre><code>
clear_trans() = {clear_trans, <a href="#type-clear_reason">clear_reason()</a>}
</code></pre>




### <a name="type-effect">effect()</a> ###


<pre><code>
effect() = <a href="#type-clear_trans">clear_trans()</a> | <a href="#type-tu_result">tu_result()</a> | <a href="#type-send_request">send_request()</a> | <a href="#type-send_response">send_response()</a> | <a href="#type-set_timer">set_timer()</a>
</code></pre>




### <a name="type-send_request">send_request()</a> ###


<pre><code>
send_request() = {send_request, <a href="ersip_request.md#type-request">ersip_request:request()</a>}
</code></pre>




### <a name="type-send_response">send_response()</a> ###


<pre><code>
send_response() = {send_response, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}
</code></pre>




### <a name="type-set_timer">set_timer()</a> ###


<pre><code>
set_timer() = {set_timer, {timeout(), TimerEv::<a href="#type-timer_event">timer_event()</a>}}
</code></pre>




### <a name="type-timer_event">timer_event()</a> ###


<pre><code>
timer_event() = term()
</code></pre>




### <a name="type-tu_result">tu_result()</a> ###


<pre><code>
tu_result() = {tu_result, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#clear_trans-1">clear_trans/1</a></td><td>Delete transaction.</td></tr><tr><td valign="top"><a href="#send_request-1">send_request/1</a></td><td>Send request message.</td></tr><tr><td valign="top"><a href="#send_response-1">send_response/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_timer-2">set_timer/2</a></td><td>Set timer for specified time interval.</td></tr><tr><td valign="top"><a href="#tu_result-1">tu_result/1</a></td><td>Inform transaction user about transaction result.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="clear_trans-1"></a>

### clear_trans/1 ###

<pre><code>
clear_trans(Reason::<a href="#type-clear_reason">clear_reason()</a>) -&gt; <a href="#type-clear_trans">clear_trans()</a>
</code></pre>
<br />

Delete transaction. Transaction must be removed if it saved
somewhere.

<a name="send_request-1"></a>

### send_request/1 ###

<pre><code>
send_request(Request) -&gt; <a href="#type-send_request">send_request()</a>
</code></pre>

<ul class="definitions"><li><code>Request = <a href="ersip_request.md#type-request">ersip_request:request()</a></code></li></ul>

Send request message.

<a name="send_response-1"></a>

### send_response/1 ###

<pre><code>
send_response(Resp) -&gt; <a href="#type-send_response">send_response()</a>
</code></pre>

<ul class="definitions"><li><code>Resp = <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a></code></li></ul>

<a name="set_timer-2"></a>

### set_timer/2 ###

<pre><code>
set_timer(Timeout::timeout(), TimerEv::<a href="#type-timer_event">timer_event()</a>) -&gt; <a href="#type-set_timer">set_timer()</a>
</code></pre>
<br />

Set timer for specified time interval. After timeout is
expired TimerFun must be called to process timer event.

<a name="tu_result-1"></a>

### tu_result/1 ###

<pre><code>
tu_result(SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="#type-tu_result">tu_result()</a>
</code></pre>
<br />

Inform transaction user about transaction result.

