

# Module ersip_sdp_conn #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-conn">conn()</a> ###


<pre><code>
conn() = #conn{addr = <a href="ersip_sdp_addr.md#type-addr">ersip_sdp_addr:addr()</a>, ttl = non_neg_integer() | undefined, num_addrs = non_neg_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#addr-1">addr/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#num_addrs-1">num_addrs/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_addr-2">set_addr/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_num_addrs-2">set_num_addrs/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_ttl-2">set_ttl/2</a></td><td></td></tr><tr><td valign="top"><a href="#ttl-1">ttl/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="addr-1"></a>

### addr/1 ###

<pre><code>
addr(Conn::<a href="#type-conn">conn()</a>) -&gt; <a href="ersip_sdp_addr.md#type-addr">ersip_sdp_addr:addr()</a>
</code></pre>
<br />

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Conn::undefined | <a href="#type-conn">conn()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="num_addrs-1"></a>

### num_addrs/1 ###

<pre><code>
num_addrs(Conn::<a href="#type-conn">conn()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Other::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-conn">conn()</a> | undefined)
</code></pre>
<br />

<a name="set_addr-2"></a>

### set_addr/2 ###

<pre><code>
set_addr(Addr::<a href="ersip_sdp_addr.md#type-addr">ersip_sdp_addr:addr()</a>, Conn::<a href="#type-conn">conn()</a>) -&gt; <a href="#type-conn">conn()</a>
</code></pre>
<br />

<a name="set_num_addrs-2"></a>

### set_num_addrs/2 ###

<pre><code>
set_num_addrs(NumAddrs::non_neg_integer(), Conn::<a href="#type-conn">conn()</a>) -&gt; <a href="#type-conn">conn()</a>
</code></pre>
<br />

<a name="set_ttl-2"></a>

### set_ttl/2 ###

<pre><code>
set_ttl(TTL::non_neg_integer() | undefined, Conn::<a href="#type-conn">conn()</a>) -&gt; <a href="#type-conn">conn()</a>
</code></pre>
<br />

<a name="ttl-1"></a>

### ttl/1 ###

<pre><code>
ttl(Conn::<a href="#type-conn">conn()</a>) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

