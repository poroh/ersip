

# Module ersip_conn #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-options">options()</a> ###


<pre><code>
options() = map()
</code></pre>




### <a name="type-result">result()</a> ###


<pre><code>
result() = {<a href="#type-sip_conn">sip_conn()</a>, [<a href="ersip_conn_se.md#type-side_effect">ersip_conn_se:side_effect()</a>]}
</code></pre>




### <a name="type-sip_conn">sip_conn()</a> ###


<pre><code>
sip_conn() = #sip_conn{local_addr = {<a href="ersip_host.md#type-host">ersip_host:host()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}, remote_addr = {<a href="ersip_host.md#type-host">ersip_host:host()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}, transport = <a href="ersip_transport.md#type-transport">ersip_transport:transport()</a>, options = <a href="#type-options">options()</a>, parser = <a href="ersip_parser.md#type-data">ersip_parser:data()</a> | undefined}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_via-3">add_via/3</a></td><td></td></tr><tr><td valign="top"><a href="#conn_data-2">conn_data/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-6">new/6</a></td><td></td></tr><tr><td valign="top"><a href="#source-1">source/1</a></td><td></td></tr><tr><td valign="top"><a href="#take_via-2">take_via/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_via-3"></a>

### add_via/3 ###

<pre><code>
add_via(Msg::<a href="ersip_msg.md#type-message">ersip_msg:message()</a>, Branch::<a href="ersip_branch.md#type-branch">ersip_branch:branch()</a>, Sip_conn::<a href="#type-sip_conn">sip_conn()</a>) -&gt; <a href="ersip_msg.md#type-message">ersip_msg:message()</a>
</code></pre>
<br />

<a name="conn_data-2"></a>

### conn_data/2 ###

<pre><code>
conn_data(Binary::binary(), Sip_conn::<a href="#type-sip_conn">sip_conn()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="new-6"></a>

### new/6 ###

<pre><code>
new(LocalAddr, LocalPort, RemoteAddr, RemotePort, SIPTransport, Options) -&gt; <a href="#type-sip_conn">sip_conn()</a>
</code></pre>

<ul class="definitions"><li><code>LocalAddr = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>LocalPort = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>RemoteAddr = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>RemotePort = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>SIPTransport = <a href="ersip_transport.md#type-transport">ersip_transport:transport()</a></code></li><li><code>Options = <a href="#type-options">options()</a></code></li></ul>

<a name="source-1"></a>

### source/1 ###

<pre><code>
source(Sip_conn::<a href="#type-sip_conn">sip_conn()</a>) -&gt; <a href="ersip_source.md#type-source">ersip_source:source()</a>
</code></pre>
<br />

<a name="take_via-2"></a>

### take_via/2 ###

<pre><code>
take_via(Msg::<a href="ersip_msg.md#type-message">ersip_msg:message()</a>, Sip_conn::<a href="#type-sip_conn">sip_conn()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, <a href="ersip_hdr_via.md#type-via">ersip_hdr_via:via()</a>, <a href="ersip_msg.md#type-message">ersip_msg:message()</a>} | {error, no_via} | {error, {bad_via, term()}} | {error, {via_mismatch, binary(), binary()}}</code></li></ul>

