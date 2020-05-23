

# Module ersip_host #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-address">address()</a> ###


<pre><code>
address() = {ipv4, <a href="inet.md#type-ip4_address">inet:ip4_address()</a>} | {ipv6, <a href="inet.md#type-ip6_address">inet:ip6_address()</a>}
</code></pre>




### <a name="type-host">host()</a> ###


<pre><code>
host() = <a href="#type-hostname">hostname()</a> | <a href="#type-address">address()</a>
</code></pre>




### <a name="type-hostname">hostname()</a> ###


<pre><code>
hostname() = {hostname, binary()}
</code></pre>




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = {invalid_name, binary()} | {invalid_ipv6, binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Assemble hostname to iolist().</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Assemble hostname to binary().</td></tr><tr><td valign="top"><a href="#assemble_received-1">assemble_received/1</a></td><td>Assemble host as received parameter of via.</td></tr><tr><td valign="top"><a href="#check_hostname-1">check_hostname/1</a></td><td>Check that hostname is valid domain name.</td></tr><tr><td valign="top"><a href="#ip_address-1">ip_address/1</a></td><td>Transform host to inet:ip_address().</td></tr><tr><td valign="top"><a href="#is_host-1">is_host/1</a></td><td>Check term is valid host.</td></tr><tr><td valign="top"><a href="#is_ip_address-1">is_ip_address/1</a></td><td>Check if host is defined as IP address.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Create hostname from inet:ip_address() or from another host or
from binary().</td></tr><tr><td valign="top"><a href="#make_key-1">make_key/1</a></td><td>Make comparable hostname (from rfc3261 comparision rules).</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Generate host specification from binary.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(X1::<a href="#type-host">host()</a>) -&gt; iolist()
</code></pre>
<br />

Assemble hostname to iolist().

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Host::<a href="#type-host">host()</a>) -&gt; binary()
</code></pre>
<br />

Assemble hostname to binary().

<a name="assemble_received-1"></a>

### assemble_received/1 ###

<pre><code>
assemble_received(X1::<a href="#type-host">host()</a>) -&gt; iolist()
</code></pre>
<br />

Assemble host as received parameter of via.

```
  via-received      =  "received" EQUAL (IPv4address / IPv6address)
```

<a name="check_hostname-1"></a>

### check_hostname/1 ###

<pre><code>
check_hostname(Bin::binary()) -&gt; boolean()
</code></pre>
<br />

Check that hostname is valid domain name.

<a name="ip_address-1"></a>

### ip_address/1 ###

<pre><code>
ip_address(X1::<a href="#type-host">host()</a>) -&gt; <a href="inet.md#type-ip_address">inet:ip_address()</a>
</code></pre>
<br />

Transform host to inet:ip_address().
Raises error if not ersip_host:is_ip_address().

<a name="is_host-1"></a>

### is_host/1 ###

<pre><code>
is_host(X1::<a href="#type-host">host()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Check term is valid host.

<a name="is_ip_address-1"></a>

### is_ip_address/1 ###

<pre><code>
is_ip_address(X1::<a href="#type-host">host()</a>) -&gt; boolean()
</code></pre>
<br />

Check if host is defined as IP address.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Addr::<a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="#type-host">host()</a> | binary()) -&gt; <a href="#type-host">host()</a>
</code></pre>
<br />

Create hostname from inet:ip_address() or from another host or
from binary().

<a name="make_key-1"></a>

### make_key/1 ###

<pre><code>
make_key(H::<a href="#type-host">host()</a>) -&gt; <a href="#type-host">host()</a>
</code></pre>
<br />

Make comparable hostname (from rfc3261 comparision rules).

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Binary::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-host">host()</a>)
</code></pre>
<br />

Generate host specification from binary.

