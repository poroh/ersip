

# Module ersip_source #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-source">source()</a> ###


<pre><code>
source() = #source{local = {<a href="ersip_host.md#type-host">ersip_host:host()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}, peer = {<a href="ersip_host.md#type-host">ersip_host:host()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}, transport = <a href="ersip_transport.md#type-transport">ersip_transport:transport()</a>, source_id = term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#is_tls-1">is_tls/1</a></td><td></td></tr><tr><td valign="top"><a href="#local-1">local/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td></td></tr><tr><td valign="top"><a href="#remote-1">remote/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_source_id-2">set_source_id/2</a></td><td></td></tr><tr><td valign="top"><a href="#source_id-1">source_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#transport-1">transport/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="is_tls-1"></a>

### is_tls/1 ###

<pre><code>
is_tls(Source::<a href="#type-source">source()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="local-1"></a>

### local/1 ###

<pre><code>
local(Source::<a href="#type-source">source()</a>) -&gt; {<a href="ersip_host.md#type-host">ersip_host:host()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}
</code></pre>
<br />

<a name="new-4"></a>

### new/4 ###

<pre><code>
new(Local, Peer, Transport::<a href="ersip_transport.md#type-transport">ersip_transport:transport()</a>, SourceId) -&gt; <a href="#type-source">source()</a>
</code></pre>

<ul class="definitions"><li><code>Local = {<a href="ersip_host.md#type-host">ersip_host:host()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}</code></li><li><code>Peer = {<a href="ersip_host.md#type-host">ersip_host:host()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}</code></li><li><code>SourceId = term()</code></li></ul>

<a name="remote-1"></a>

### remote/1 ###

<pre><code>
remote(Source::<a href="#type-source">source()</a>) -&gt; {<a href="ersip_host.md#type-host">ersip_host:host()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}
</code></pre>
<br />

<a name="set_source_id-2"></a>

### set_source_id/2 ###

<pre><code>
set_source_id(SID::term(), Source::<a href="#type-source">source()</a>) -&gt; <a href="#type-source">source()</a>
</code></pre>
<br />

<a name="source_id-1"></a>

### source_id/1 ###

<pre><code>
source_id(Source::<a href="#type-source">source()</a>) -&gt; term()
</code></pre>
<br />

<a name="transport-1"></a>

### transport/1 ###

<pre><code>
transport(Source::<a href="#type-source">source()</a>) -&gt; <a href="ersip_transport.md#type-transport">ersip_transport:transport()</a>
</code></pre>
<br />

