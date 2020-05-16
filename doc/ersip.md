

# Module ersip #
* [Data Types](#types)

<a name="types"></a>

## Data Types ##




### <a name="type-connection_id">connection_id()</a> ###


<pre><code>
connection_id() = term()
</code></pre>




### <a name="type-message">message()</a> ###


<pre><code>
message() = <a href="ersip_msg.md#type-message">ersip_msg:message()</a>
</code></pre>




### <a name="type-nexthop">nexthop()</a> ###


<pre><code>
nexthop() = {peer, <a href="ersip_host.md#type-host">ersip_host:host()</a>, Port::0..65536, <a href="#type-transport">transport()</a>} | {conn, <a href="#type-connection_id">connection_id()</a>}
</code></pre>




### <a name="type-sip_options">sip_options()</a> ###


<pre><code>
sip_options() = #{sip_t1 =&gt; pos_integer(), sip_t2 =&gt; pos_integer(), sip_t4 =&gt; pos_integer(), trans_expire =&gt; pos_integer()}
</code></pre>




### <a name="type-transport">transport()</a> ###


<pre><code>
transport() = {transport, <a href="#type-transport_atom">transport_atom()</a>}
</code></pre>




### <a name="type-transport_atom">transport_atom()</a> ###


<pre><code>
transport_atom() = udp | tcp | tls | ws | wss | sctp
</code></pre>




### <a name="type-uri">uri()</a> ###


<pre><code>
uri() = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>

