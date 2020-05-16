

# Module ersip_response #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-target">target()</a> ###


<pre><code>
target() = {<a href="ersip_host.md#type-host">ersip_host:host()</a>, <a href="ersip_transport.md#type-port_number">ersip_transport:port_number()</a>, <a href="ersip_transport.md#type-transport">ersip_transport:transport()</a>, Options::map()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#target-1">target/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="target-1"></a>

### target/1 ###

<pre><code>
target(Via::<a href="ersip_hdr_via.md#type-via">ersip_hdr_via:via()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {direct, <a href="#type-target">target()</a>} | {reuse, <a href="#type-target">target()</a>}</code></li></ul>

