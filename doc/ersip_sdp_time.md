

# Module ersip_sdp_time #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-ntp_timestamp">ntp_timestamp()</a> ###


<pre><code>
ntp_timestamp() = non_neg_integer()
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result(X) = <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(X)
</code></pre>




### <a name="type-sess_item">sess_item()</a> ###


<pre><code>
sess_item() = #sess_item{start = <a href="#type-ntp_timestamp">ntp_timestamp()</a>, stop = <a href="#type-ntp_timestamp">ntp_timestamp()</a>, repeat = [binary()]}
</code></pre>




### <a name="type-timings">timings()</a> ###


<pre><code>
timings() = #timings{items = [<a href="#type-sess_item">sess_item()</a>, ...], zone = binary() | undefined}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Timings::<a href="#type-timings">timings()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary()) -&gt; <a href="#type-parse_result">parse_result</a>(<a href="#type-timings">timings()</a>)
</code></pre>
<br />

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Timings::<a href="#type-timings">timings()</a>) -&gt; <a href="#type-ntp_timestamp">ntp_timestamp()</a>
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Timings::<a href="#type-timings">timings()</a>) -&gt; <a href="#type-ntp_timestamp">ntp_timestamp()</a>
</code></pre>
<br />

