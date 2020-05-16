

# Module ersip_sdp #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-maybe_binary">maybe_binary()</a> ###


<pre><code>
maybe_binary() = binary() | undefined
</code></pre>




### <a name="type-sdp">sdp()</a> ###


<pre><code>
sdp() = #sdp{origin = <a href="ersip_sdp_origin.md#type-origin">ersip_sdp_origin:origin()</a>, session_name = binary(), info = <a href="#type-maybe_binary">maybe_binary()</a>, uri = <a href="#type-maybe_binary">maybe_binary()</a>, emails = [binary()], phones = [binary()], conn = <a href="ersip_sdp_conn.md#type-conn">ersip_sdp_conn:conn()</a> | undefined, bandwidth = <a href="ersip_sdp_bandwidth.md#type-bandwidth">ersip_sdp_bandwidth:bandwidth()</a>, timings = <a href="ersip_sdp_time.md#type-timings">ersip_sdp_time:timings()</a>, key = <a href="#type-maybe_binary">maybe_binary()</a>, attrs = <a href="ersip_sdp_attr.md#type-attr_list">ersip_sdp_attr:attr_list()</a>, medias = [<a href="ersip_sdp_media.md#type-media">ersip_sdp_media:media()</a>]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#attrs-1">attrs/1</a></td><td></td></tr><tr><td valign="top"><a href="#bandwidth-1">bandwidth/1</a></td><td></td></tr><tr><td valign="top"><a href="#conn-1">conn/1</a></td><td></td></tr><tr><td valign="top"><a href="#emails-1">emails/1</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td></td></tr><tr><td valign="top"><a href="#medias-1">medias/1</a></td><td></td></tr><tr><td valign="top"><a href="#origin-1">origin/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#phones-1">phones/1</a></td><td></td></tr><tr><td valign="top"><a href="#session_name-1">session_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_attrs-2">set_attrs/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_conn-2">set_conn/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_medias-2">set_medias/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_origin-2">set_origin/2</a></td><td></td></tr><tr><td valign="top"><a href="#time-1">time/1</a></td><td></td></tr><tr><td valign="top"><a href="#uri-1">uri/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; binary()
</code></pre>
<br />

<a name="attrs-1"></a>

### attrs/1 ###

<pre><code>
attrs(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; <a href="ersip_sdp_attr.md#type-attr_list">ersip_sdp_attr:attr_list()</a>
</code></pre>
<br />

<a name="bandwidth-1"></a>

### bandwidth/1 ###

<pre><code>
bandwidth(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; <a href="ersip_sdp_bandwidth.md#type-bandwidth">ersip_sdp_bandwidth:bandwidth()</a>
</code></pre>
<br />

<a name="conn-1"></a>

### conn/1 ###

<pre><code>
conn(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; <a href="ersip_sdp_conn.md#type-conn">ersip_sdp_conn:conn()</a> | undefined
</code></pre>
<br />

<a name="emails-1"></a>

### emails/1 ###

<pre><code>
emails(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; [binary()]
</code></pre>
<br />

<a name="info-1"></a>

### info/1 ###

<pre><code>
info(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; <a href="#type-maybe_binary">maybe_binary()</a>
</code></pre>
<br />

<a name="medias-1"></a>

### medias/1 ###

<pre><code>
medias(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; [<a href="ersip_sdp_media.md#type-media">ersip_sdp_media:media()</a>]
</code></pre>
<br />

<a name="origin-1"></a>

### origin/1 ###

<pre><code>
origin(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; <a href="ersip_sdp_origin.md#type-origin">ersip_sdp_origin:origin()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary()) -&gt; {ok, <a href="#type-sdp">sdp()</a>} | {error, term()}
</code></pre>
<br />

<a name="phones-1"></a>

### phones/1 ###

<pre><code>
phones(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; [binary()]
</code></pre>
<br />

<a name="session_name-1"></a>

### session_name/1 ###

<pre><code>
session_name(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; binary()
</code></pre>
<br />

<a name="set_attrs-2"></a>

### set_attrs/2 ###

<pre><code>
set_attrs(A::<a href="ersip_sdp_attr.md#type-attr_list">ersip_sdp_attr:attr_list()</a>, Sdp::<a href="#type-sdp">sdp()</a>) -&gt; <a href="#type-sdp">sdp()</a>
</code></pre>
<br />

<a name="set_conn-2"></a>

### set_conn/2 ###

<pre><code>
set_conn(Conn::<a href="ersip_sdp_conn.md#type-conn">ersip_sdp_conn:conn()</a>, Sdp::<a href="#type-sdp">sdp()</a>) -&gt; <a href="#type-sdp">sdp()</a>
</code></pre>
<br />

<a name="set_medias-2"></a>

### set_medias/2 ###

<pre><code>
set_medias(Medias::[<a href="ersip_sdp_media.md#type-media">ersip_sdp_media:media()</a>], Sdp::<a href="#type-sdp">sdp()</a>) -&gt; <a href="#type-sdp">sdp()</a>
</code></pre>
<br />

<a name="set_origin-2"></a>

### set_origin/2 ###

<pre><code>
set_origin(Origin::<a href="ersip_sdp_origin.md#type-origin">ersip_sdp_origin:origin()</a>, Sdp::<a href="#type-sdp">sdp()</a>) -&gt; <a href="#type-sdp">sdp()</a>
</code></pre>
<br />

<a name="time-1"></a>

### time/1 ###

<pre><code>
time(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; <a href="ersip_sdp_time.md#type-timings">ersip_sdp_time:timings()</a>
</code></pre>
<br />

<a name="uri-1"></a>

### uri/1 ###

<pre><code>
uri(Sdp::<a href="#type-sdp">sdp()</a>) -&gt; <a href="#type-maybe_binary">maybe_binary()</a>
</code></pre>
<br />

