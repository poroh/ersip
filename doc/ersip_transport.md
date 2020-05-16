

# Module ersip_transport #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-known_transport">known_transport()</a> ###


<pre><code>
known_transport(X) = {transport, X}
</code></pre>




### <a name="type-known_transport">known_transport()</a> ###


<pre><code>
known_transport() = {transport, <a href="#type-transport_atom">transport_atom()</a>}
</code></pre>




### <a name="type-other_transport">other_transport()</a> ###


<pre><code>
other_transport() = {other_transport, binary()}
</code></pre>




### <a name="type-port_number">port_number()</a> ###


<pre><code>
port_number() = 0..65535 | {default_port, <a href="#type-transport">transport()</a>}
</code></pre>




### <a name="type-transport">transport()</a> ###


<pre><code>
transport() = <a href="#type-known_transport">known_transport()</a> | <a href="#type-other_transport">other_transport()</a>
</code></pre>




### <a name="type-transport_atom">transport_atom()</a> ###


<pre><code>
transport_atom() = udp | tcp | tls | ws | wss | sctp
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_upper-1">assemble_upper/1</a></td><td></td></tr><tr><td valign="top"><a href="#default_port-1">default_port/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_known_transport-1">is_known_transport/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_message_oriented-1">is_message_oriented/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_reliable-1">is_reliable/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_tls-1">is_tls/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_by_uri-1">make_by_uri/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_port_number-1">parse_port_number/1</a></td><td></td></tr><tr><td valign="top"><a href="#tcp-0">tcp/0</a></td><td></td></tr><tr><td valign="top"><a href="#tls-0">tls/0</a></td><td></td></tr><tr><td valign="top"><a href="#udp-0">udp/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(T::<a href="#type-transport">transport()</a>) -&gt; binary()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(X1::<a href="#type-transport">transport()</a>) -&gt; binary()
</code></pre>
<br />

<a name="assemble_upper-1"></a>

### assemble_upper/1 ###

<pre><code>
assemble_upper(X1::<a href="#type-transport">transport()</a>) -&gt; binary()
</code></pre>
<br />

<a name="default_port-1"></a>

### default_port/1 ###

<pre><code>
default_port(T::<a href="#type-transport">transport()</a>) -&gt; 0..65535 | {default_port, <a href="#type-transport">transport()</a>}
</code></pre>
<br />

<a name="is_known_transport-1"></a>

### is_known_transport/1 ###

<pre><code>
is_known_transport(X1::<a href="#type-transport">transport()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="is_message_oriented-1"></a>

### is_message_oriented/1 ###

<pre><code>
is_message_oriented(X1::<a href="#type-known_transport">known_transport()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="is_reliable-1"></a>

### is_reliable/1 ###

<pre><code>
is_reliable(X1::<a href="#type-known_transport">known_transport()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="is_tls-1"></a>

### is_tls/1 ###

<pre><code>
is_tls(X1::<a href="#type-known_transport">known_transport()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(V::binary() | <a href="#type-transport_atom">transport_atom()</a>) -&gt; <a href="#type-transport">transport()</a>
</code></pre>
<br />

<a name="make_by_uri-1"></a>

### make_by_uri/1 ###

<pre><code>
make_by_uri(URI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-transport">transport()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(V::binary() | <a href="#type-transport_atom">transport_atom()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, <a href="#type-transport">transport()</a>} | {error, ErrorReason}</code></li><li><code>ErrorReason = {bad_transport_atom, atom()} | {einval, transport}</code></li></ul>

<a name="parse_port_number-1"></a>

### parse_port_number/1 ###

<pre><code>
parse_port_number(Bin::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-port_number">port_number()</a>)
</code></pre>
<br />

<a name="tcp-0"></a>

### tcp/0 ###

<pre><code>
tcp() -&gt; <a href="#type-known_transport">known_transport</a>(tcp)
</code></pre>
<br />

<a name="tls-0"></a>

### tls/0 ###

<pre><code>
tls() -&gt; <a href="#type-known_transport">known_transport</a>(tls)
</code></pre>
<br />

<a name="udp-0"></a>

### udp/0 ###

<pre><code>
udp() -&gt; <a href="#type-known_transport">known_transport</a>(udp)
</code></pre>
<br />

