

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




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = {bad_transport_atom, atom()} | {invalid_transport, binary()}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-transport">transport()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = binary()
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Assemble transport as iolist().</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Assemble transport as binary().</td></tr><tr><td valign="top"><a href="#assemble_upper-1">assemble_upper/1</a></td><td>Assemble transport as upper-case transport (for Via header).</td></tr><tr><td valign="top"><a href="#default_port-1">default_port/1</a></td><td>Default port for the defined transport.</td></tr><tr><td valign="top"><a href="#is_known_transport-1">is_known_transport/1</a></td><td>Check if transport is known transport.</td></tr><tr><td valign="top"><a href="#is_message_oriented-1">is_message_oriented/1</a></td><td>Check if transport is message (datagram) oriented.</td></tr><tr><td valign="top"><a href="#is_reliable-1">is_reliable/1</a></td><td>Check if transport has reliable delivery.</td></tr><tr><td valign="top"><a href="#is_tls-1">is_tls/1</a></td><td>Check if transport is TLS-based (secure).</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Create transport from binary or from raw representation.</td></tr><tr><td valign="top"><a href="#make_by_uri-1">make_by_uri/1</a></td><td>Get transport from URI.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse transport from raw representation or from binary().</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Get raw value (in plain erlang types) of the transport.</td></tr><tr><td valign="top"><a href="#sctp-0">sctp/0</a></td><td>Construct WSs transport.</td></tr><tr><td valign="top"><a href="#tcp-0">tcp/0</a></td><td>Construct TCP transport.</td></tr><tr><td valign="top"><a href="#tls-0">tls/0</a></td><td>Construct TLS transport.</td></tr><tr><td valign="top"><a href="#udp-0">udp/0</a></td><td>Construct UDP transport.</td></tr><tr><td valign="top"><a href="#ws-0">ws/0</a></td><td>Construct WS transport.</td></tr><tr><td valign="top"><a href="#wss-0">wss/0</a></td><td>Construct WSs transport.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(T::<a href="#type-transport">transport()</a>) -&gt; binary()
</code></pre>
<br />

Assemble transport as iolist().

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(X1::<a href="#type-transport">transport()</a>) -&gt; binary()
</code></pre>
<br />

Assemble transport as binary().

<a name="assemble_upper-1"></a>

### assemble_upper/1 ###

<pre><code>
assemble_upper(X1::<a href="#type-transport">transport()</a>) -&gt; binary()
</code></pre>
<br />

Assemble transport as upper-case transport (for Via header).

<a name="default_port-1"></a>

### default_port/1 ###

<pre><code>
default_port(X1::<a href="#type-transport">transport()</a>) -&gt; 0..65535 | {default_port, <a href="#type-transport">transport()</a>}
</code></pre>
<br />

Default port for the defined transport.

<a name="is_known_transport-1"></a>

### is_known_transport/1 ###

<pre><code>
is_known_transport(X1::<a href="#type-transport">transport()</a>) -&gt; boolean()
</code></pre>
<br />

Check if transport is known transport.

<a name="is_message_oriented-1"></a>

### is_message_oriented/1 ###

<pre><code>
is_message_oriented(X1::<a href="#type-known_transport">known_transport()</a>) -&gt; boolean()
</code></pre>
<br />

Check if transport is message (datagram) oriented.
Mostly it is require for connection-level parsers. Wether it needs
to parse stream or not.

<a name="is_reliable-1"></a>

### is_reliable/1 ###

<pre><code>
is_reliable(X1::<a href="#type-known_transport">known_transport()</a>) -&gt; boolean()
</code></pre>
<br />

Check if transport has reliable delivery.

<a name="is_tls-1"></a>

### is_tls/1 ###

<pre><code>
is_tls(X1::<a href="#type-known_transport">known_transport()</a>) -&gt; boolean()
</code></pre>
<br />

Check if transport is TLS-based (secure).

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(V::binary() | <a href="#type-transport_atom">transport_atom()</a>) -&gt; <a href="#type-transport">transport()</a>
</code></pre>
<br />

Create transport from binary or from raw representation.
Examples:

```
  TCP = ersip_transport:tcp(),
  TCP == ersip_transport:make(<<"tcp">>),
  TCP == ersip_transport:make(tcp).
```

<a name="make_by_uri-1"></a>

### make_by_uri/1 ###

<pre><code>
make_by_uri(URI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-transport">transport()</a>
</code></pre>
<br />

Get transport from URI.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(V::binary() | <a href="#type-raw">raw()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

Parse transport from raw representation or from binary().

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Transport::<a href="#type-transport">transport()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Get raw value (in plain erlang types) of the transport.

<a name="sctp-0"></a>

### sctp/0 ###

<pre><code>
sctp() -&gt; <a href="#type-known_transport">known_transport</a>(sctp)
</code></pre>
<br />

Construct WSs transport.

<a name="tcp-0"></a>

### tcp/0 ###

<pre><code>
tcp() -&gt; <a href="#type-known_transport">known_transport</a>(tcp)
</code></pre>
<br />

Construct TCP transport.

<a name="tls-0"></a>

### tls/0 ###

<pre><code>
tls() -&gt; <a href="#type-known_transport">known_transport</a>(tls)
</code></pre>
<br />

Construct TLS transport.

<a name="udp-0"></a>

### udp/0 ###

<pre><code>
udp() -&gt; <a href="#type-known_transport">known_transport</a>(udp)
</code></pre>
<br />

Construct UDP transport.

<a name="ws-0"></a>

### ws/0 ###

<pre><code>
ws() -&gt; <a href="#type-known_transport">known_transport</a>(ws)
</code></pre>
<br />

Construct WS transport.

<a name="wss-0"></a>

### wss/0 ###

<pre><code>
wss() -&gt; <a href="#type-known_transport">known_transport</a>(wss)
</code></pre>
<br />

Construct WSs transport.

