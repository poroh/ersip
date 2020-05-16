

# Module ersip_sdp_addr #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-addr">addr()</a> ###


<pre><code>
addr() = {ip4, <a href="inet.md#type-ip4_address">inet:ip4_address()</a>} | {ip4_host, <a href="#type-fqdn">fqdn()</a>} | {ip6, <a href="inet.md#type-ip6_address">inet:ip6_address()</a>} | {ip6_host, <a href="#type-fqdn">fqdn()</a>} | {unknown_addr, binary(), binary(), binary()}
</code></pre>




### <a name="type-fqdn">fqdn()</a> ###


<pre><code>
fqdn() = binary()
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-addr">addr()</a>} | {error, term()}
</code></pre>




### <a name="type-raw_addr">raw_addr()</a> ###


<pre><code>
raw_addr() = {binary(), binary(), binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#from_raw-1">from_raw/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-3">parse/3</a></td><td></td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(X1::<a href="#type-addr">addr()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="from_raw-1"></a>

### from_raw/1 ###

<pre><code>
from_raw(X1::<a href="#type-raw_addr">raw_addr()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-addr">addr()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-addr">addr()</a>)
</code></pre>
<br />

<a name="parse-3"></a>

### parse/3 ###

<pre><code>
parse(NetType::binary(), AddrType::binary(), Address::binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(X1::<a href="#type-addr">addr()</a>) -&gt; <a href="#type-raw_addr">raw_addr()</a>
</code></pre>
<br />

