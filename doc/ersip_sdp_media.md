

# Module ersip_sdp_media #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-maybe_binary">maybe_binary()</a> ###


<pre><code>
maybe_binary() = binary() | undefined
</code></pre>




### <a name="type-media">media()</a> ###


<pre><code>
media() = #media{type = <a href="#type-media_type">media_type()</a>, port = <a href="inet.md#type-port_number">inet:port_number()</a>, port_num = non_neg_integer() | undefined, protocol = [binary(), ...], fmts = [binary()], title = <a href="#type-maybe_binary">maybe_binary()</a>, conn = <a href="ersip_sdp_conn.md#type-conn">ersip_sdp_conn:conn()</a> | undefined, bandwidth = <a href="ersip_sdp_bandwidth.md#type-bandwidth">ersip_sdp_bandwidth:bandwidth()</a>, key = <a href="#type-maybe_binary">maybe_binary()</a>, attrs = <a href="ersip_sdp_attr.md#type-attr_list">ersip_sdp_attr:attr_list()</a>}
</code></pre>




### <a name="type-media_type">media_type()</a> ###


<pre><code>
media_type() = binary()
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>([<a href="#type-media">media()</a>])
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#attrs-1">attrs/1</a></td><td></td></tr><tr><td valign="top"><a href="#conn-1">conn/1</a></td><td></td></tr><tr><td valign="top"><a href="#formats-1">formats/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#port-1">port/1</a></td><td></td></tr><tr><td valign="top"><a href="#port_num-1">port_num/1</a></td><td></td></tr><tr><td valign="top"><a href="#protocol-1">protocol/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_attrs-2">set_attrs/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_conn-2">set_conn/2</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Medias::[<a href="#type-media">media()</a>]) -&gt; iolist()
</code></pre>
<br />

<a name="attrs-1"></a>

### attrs/1 ###

<pre><code>
attrs(Media::<a href="#type-media">media()</a>) -&gt; <a href="ersip_sdp_attr.md#type-attr_list">ersip_sdp_attr:attr_list()</a>
</code></pre>
<br />

<a name="conn-1"></a>

### conn/1 ###

<pre><code>
conn(Media::<a href="#type-media">media()</a>) -&gt; <a href="ersip_sdp_conn.md#type-conn">ersip_sdp_conn:conn()</a> | undefined
</code></pre>
<br />

<a name="formats-1"></a>

### formats/1 ###

<pre><code>
formats(Media::<a href="#type-media">media()</a>) -&gt; [binary()]
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

<a name="port-1"></a>

### port/1 ###

<pre><code>
port(Media::<a href="#type-media">media()</a>) -&gt; <a href="inet.md#type-port_number">inet:port_number()</a>
</code></pre>
<br />

<a name="port_num-1"></a>

### port_num/1 ###

<pre><code>
port_num(Media::<a href="#type-media">media()</a>) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

<a name="protocol-1"></a>

### protocol/1 ###

<pre><code>
protocol(Media::<a href="#type-media">media()</a>) -&gt; [binary(), ...]
</code></pre>
<br />

<a name="set_attrs-2"></a>

### set_attrs/2 ###

<pre><code>
set_attrs(A::<a href="ersip_sdp_attr.md#type-attr_list">ersip_sdp_attr:attr_list()</a>, Media::<a href="#type-media">media()</a>) -&gt; <a href="#type-media">media()</a>
</code></pre>
<br />

<a name="set_conn-2"></a>

### set_conn/2 ###

<pre><code>
set_conn(Conn::<a href="ersip_sdp_conn.md#type-conn">ersip_sdp_conn:conn()</a>, Media::<a href="#type-media">media()</a>) -&gt; <a href="#type-media">media()</a>
</code></pre>
<br />

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(Media::<a href="#type-media">media()</a>) -&gt; binary()
</code></pre>
<br />

