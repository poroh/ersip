

# Module ersip_sdp_origin #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-origin">origin()</a> ###


<pre><code>
origin() = #origin{username = binary(), sess_id = non_neg_integer(), sess_version = non_neg_integer(), address = <a href="ersip_sdp_addr.md#type-addr">ersip_sdp_addr:addr()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#address-1">address/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#session_id-1">session_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#session_version-1">session_version/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_address-2">set_address/2</a></td><td></td></tr><tr><td valign="top"><a href="#username-1">username/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="address-1"></a>

### address/1 ###

<pre><code>
address(Origin::<a href="#type-origin">origin()</a>) -&gt; <a href="ersip_sdp_addr.md#type-addr">ersip_sdp_addr:addr()</a>
</code></pre>
<br />

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Origin::<a href="#type-origin">origin()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(V::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-origin">origin()</a>)
</code></pre>
<br />

<a name="session_id-1"></a>

### session_id/1 ###

<pre><code>
session_id(Origin::<a href="#type-origin">origin()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="session_version-1"></a>

### session_version/1 ###

<pre><code>
session_version(Origin::<a href="#type-origin">origin()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="set_address-2"></a>

### set_address/2 ###

<pre><code>
set_address(Addr::<a href="ersip_sdp_addr.md#type-addr">ersip_sdp_addr:addr()</a>, Origin::<a href="#type-origin">origin()</a>) -&gt; <a href="#type-origin">origin()</a>
</code></pre>
<br />

<a name="username-1"></a>

### username/1 ###

<pre><code>
username(Origin::<a href="#type-origin">origin()</a>) -&gt; binary()
</code></pre>
<br />

