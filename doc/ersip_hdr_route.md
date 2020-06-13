

# Module ersip_hdr_route #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = #{uri =&gt; <a href="ersip_uri.md#type-raw">ersip_uri:raw()</a>, params =&gt; <a href="ersip_hparams.md#type-raw">ersip_hparams:raw()</a>, display_name =&gt; <a href="ersip_display_name.md#type-raw">ersip_display_name:raw()</a>}
</code></pre>




### <a name="type-route">route()</a> ###


<pre><code>
route() = #route{display_name = <a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>, uri = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, hparams = <a href="ersip_hparams.md#type-hparams">ersip_hparams:hparams()</a>}
</code></pre>




### <a name="type-route_param">route_param()</a> ###


<pre><code>
route_param() = {Key::binary(), Value::binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Assemble route header to iolist().</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Assemble route header to binary().</td></tr><tr><td valign="top"><a href="#display_name-1">display_name/1</a></td><td>Display name in Route header.</td></tr><tr><td valign="top"><a href="#is_loose_route-1">is_loose_route/1</a></td><td>Check if route contains loose router's URI.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Make Route header from binary or from raw representation.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Create Route header from SIP URI.</td></tr><tr><td valign="top"><a href="#param-2">param/2</a></td><td>Get parameter of Route header.</td></tr><tr><td valign="top"><a href="#params-1">params/1</a></td><td>Get parameters of Route header.</td></tr><tr><td valign="top"><a href="#parse_hdr-1">parse_hdr/1</a></td><td>Parse single Route header and return unparsed rest.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Raw representation of Route header.</td></tr><tr><td valign="top"><a href="#set_display_name-2">set_display_name/2</a></td><td>Set display name of Route header.</td></tr><tr><td valign="top"><a href="#set_param-3">set_param/3</a></td><td>Set parameters of Route header.</td></tr><tr><td valign="top"><a href="#set_uri-2">set_uri/2</a></td><td>Set URI of Route header.</td></tr><tr><td valign="top"><a href="#uri-1">uri/1</a></td><td>URI from Route header.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Route::<a href="#type-route">route()</a>) -&gt; iolist()
</code></pre>
<br />

Assemble route header to iolist().

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Route::<a href="#type-route">route()</a>) -&gt; binary()
</code></pre>
<br />

Assemble route header to binary().

<a name="display_name-1"></a>

### display_name/1 ###

<pre><code>
display_name(Route::<a href="#type-route">route()</a>) -&gt; <a href="ersip_display_name.md#type-display_name">ersip_display_name:display_name()</a>
</code></pre>
<br />

Display name in Route header.

<a name="is_loose_route-1"></a>

### is_loose_route/1 ###

<pre><code>
is_loose_route(Route::<a href="#type-route">route()</a>) -&gt; boolean()
</code></pre>
<br />

Check if route contains loose router's URI.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-route">route()</a>
</code></pre>
<br />

Make Route header from binary or from raw representation.

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(URI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-route">route()</a>
</code></pre>
<br />

Create Route header from SIP URI.

<a name="param-2"></a>

### param/2 ###

<pre><code>
param(Key::binary(), Route::<a href="#type-route">route()</a>) -&gt; {ok, binary()} | not_found
</code></pre>
<br />

Get parameter of Route header.

<a name="params-1"></a>

### params/1 ###

<pre><code>
params(Route::<a href="#type-route">route()</a>) -&gt; [<a href="#type-route_param">route_param()</a>]
</code></pre>
<br />

Get parameters of Route header.

<a name="parse_hdr-1"></a>

### parse_hdr/1 ###

<pre><code>
parse_hdr(Bin::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-route">route()</a>)
</code></pre>
<br />

Parse single Route header and return unparsed rest.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Route::<a href="#type-route">route()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Raw representation of Route header.

<a name="set_display_name-2"></a>

### set_display_name/2 ###

<pre><code>
set_display_name(DN::<a href="ersip_display_name.md#type-display_name">ersip_display_name:display_name()</a>, Route::<a href="#type-route">route()</a>) -&gt; <a href="#type-route">route()</a>
</code></pre>
<br />

Set display name of Route header.

<a name="set_param-3"></a>

### set_param/3 ###

<pre><code>
set_param(Key::binary(), Value::binary(), Route::<a href="#type-route">route()</a>) -&gt; <a href="#type-route">route()</a>
</code></pre>
<br />

Set parameters of Route header.

<a name="set_uri-2"></a>

### set_uri/2 ###

<pre><code>
set_uri(URI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, Route::<a href="#type-route">route()</a>) -&gt; <a href="#type-route">route()</a>
</code></pre>
<br />

Set URI of Route header.

<a name="uri-1"></a>

### uri/1 ###

<pre><code>
uri(Route::<a href="#type-route">route()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

URI from Route header.

