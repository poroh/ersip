

# Module ersip_hdr_route #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-route_set">route_set()</a>} | {error, term()}
</code></pre>




### <a name="type-route">route()</a> ###


<pre><code>
route() = #route{display_name = <a href="ersip_nameaddr.md#type-display_name">ersip_nameaddr:display_name()</a>, uri = <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, params = [<a href="#type-route_param">route_param()</a>]}
</code></pre>




### <a name="type-route_param">route_param()</a> ###


<pre><code>
route_param() = {Key::binary(), Value::binary()}
</code></pre>




### <a name="type-route_set">route_set()</a> ###


<pre><code>
route_set() = <a href="ersip_route_set.md#type-route_set">ersip_route_set:route_set()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_loose_route-1">is_loose_route/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_route-1">make_route/1</a></td><td></td></tr><tr><td valign="top"><a href="#params-1">params/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_param-3">set_param/3</a></td><td></td></tr><tr><td valign="top"><a href="#uri-1">uri/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), RouteSet::<a href="#type-route_set">route_set()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

<a name="is_loose_route-1"></a>

### is_loose_route/1 ###

<pre><code>
is_loose_route(Route::<a href="#type-route">route()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Binary::iolist()) -&gt; <a href="#type-route_set">route_set()</a>
</code></pre>
<br />

<a name="make_route-1"></a>

### make_route/1 ###

<pre><code>
make_route(Bin::binary() | <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="#type-route">route()</a>
</code></pre>
<br />

<a name="params-1"></a>

### params/1 ###

<pre><code>
params(Route::<a href="#type-route">route()</a>) -&gt; [<a href="#type-route_param">route_param()</a>]
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

<a name="set_param-3"></a>

### set_param/3 ###

<pre><code>
set_param(Key::binary(), Value::binary(), Route::<a href="#type-route">route()</a>) -&gt; <a href="#type-route">route()</a>
</code></pre>
<br />

<a name="uri-1"></a>

### uri/1 ###

<pre><code>
uri(Route::<a href="#type-route">route()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

