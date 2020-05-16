

# Module ersip_uri #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-absolute_uri_data">absolute_uri_data()</a> ###


<pre><code>
absolute_uri_data() = #absolute_uri_data{opaque = binary()}
</code></pre>




### <a name="type-key_value_list">key_value_list()</a> ###


<pre><code>
key_value_list() = [{binary(), binary()} | binary()]
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = #{scheme =&gt; binary(), data =&gt; binary(), sip =&gt; <a href="#type-sip_uri_raw">sip_uri_raw()</a>}
</code></pre>




### <a name="type-scheme">scheme()</a> ###


<pre><code>
scheme() = <a href="#type-uri_scheme">uri_scheme()</a>
</code></pre>




### <a name="type-sip_uri_data">sip_uri_data()</a> ###


<pre><code>
sip_uri_data() = #sip_uri_data{user = undefined | {user, binary()}, host = <a href="ersip_host.md#type-host">ersip_host:host()</a>, host_orig = binary() | undefined, port = undefined | <a href="inet.md#type-port_number">inet:port_number()</a>, params = <a href="#type-uri_params">uri_params()</a>, headers = <a href="#type-uri_headers">uri_headers()</a>}
</code></pre>




### <a name="type-sip_uri_raw">sip_uri_raw()</a> ###


<pre><code>
sip_uri_raw() = #{host =&gt; binary(), user =&gt; binary(), port =&gt; <a href="inet.md#type-port_number">inet:port_number()</a>, params =&gt; <a href="#type-key_value_list">key_value_list()</a>, headers =&gt; <a href="#type-key_value_list">key_value_list()</a>}
</code></pre>




### <a name="type-uri">uri()</a> ###


<pre><code>
uri() = #uri{scheme = <a href="#type-uri_scheme">uri_scheme()</a>, data = <a href="#type-uri_data">uri_data()</a>}
</code></pre>




### <a name="type-uri_data">uri_data()</a> ###


<pre><code>
uri_data() = <a href="#type-sip_uri_data">sip_uri_data()</a> | <a href="#type-absolute_uri_data">absolute_uri_data()</a>
</code></pre>




### <a name="type-uri_headers">uri_headers()</a> ###


<pre><code>
uri_headers() = #{binary() =&gt; binary()}
</code></pre>




### <a name="type-uri_param_name">uri_param_name()</a> ###


<pre><code>
uri_param_name() = transport | user | method | ttl | maddr | lr | binary()
</code></pre>




### <a name="type-uri_params">uri_params()</a> ###


<pre><code>
uri_params() = #{transport =&gt; <a href="ersip_transport.md#type-transport">ersip_transport:transport()</a>, maddr =&gt; <a href="ersip_host.md#type-host">ersip_host:host()</a>, ttl =&gt; 0..255, user =&gt; phone | ip | binary(), method =&gt; binary(), lr =&gt; true}
</code></pre>




### <a name="type-uri_part">uri_part()</a> ###


<pre><code>
uri_part() = <a href="#type-scheme">scheme()</a> | {user, binary()} | {host, <a href="ersip_host.md#type-host">ersip_host:host()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a> | undefined}
</code></pre>




### <a name="type-uri_part_name">uri_part_name()</a> ###


<pre><code>
uri_part_name() = scheme | user | host | port
</code></pre>




### <a name="type-uri_scheme">uri_scheme()</a> ###


<pre><code>
uri_scheme() = {scheme, sip | sips | binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_scheme-1">assemble_scheme/1</a></td><td></td></tr><tr><td valign="top"><a href="#clear_not_allowed_parts-2">clear_not_allowed_parts/2</a></td><td></td></tr><tr><td valign="top"><a href="#clear_params-1">clear_params/1</a></td><td></td></tr><tr><td valign="top"><a href="#clear_transport-1">clear_transport/1</a></td><td></td></tr><tr><td valign="top"><a href="#data-1">data/1</a></td><td>Get data of the URI (everything after scheme).</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Get URI part by identify.</td></tr><tr><td valign="top"><a href="#host-1">host/1</a></td><td>Get host part of SIP URI.</td></tr><tr><td valign="top"><a href="#host_bin-1">host_bin/1</a></td><td>Get host part of SIP URI in binary representation.</td></tr><tr><td valign="top"><a href="#is_sip-1">is_sip/1</a></td><td>Returns true if URI is SIP or SIPS URI.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_key-1">make_key/1</a></td><td>Make URI comparable with =:= erlang operator.</td></tr><tr><td valign="top"><a href="#params-1">params/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse URI from the binary
<pre>  SIP-URI          =  "sip:" [userinfo] hostport
                      uri-parameters [headers]
  SIPS-URI         =  "sips:" [userinfo] hostport
                      uri-parameters [headers]</pre></td></tr><tr><td valign="top"><a href="#port-1">port/1</a></td><td>Get port number of 'undefined'.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Get raw value (in plain erlang types) of the uri.</td></tr><tr><td valign="top"><a href="#raw_headers-1">raw_headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#raw_params-1">raw_params/1</a></td><td></td></tr><tr><td valign="top"><a href="#rebuild_header_values-1">rebuild_header_values/1</a></td><td></td></tr><tr><td valign="top"><a href="#scheme-1">scheme/1</a></td><td>Scheme of the URI.</td></tr><tr><td valign="top"><a href="#scheme_bin-1">scheme_bin/1</a></td><td>URI scheme in binary form.</td></tr><tr><td valign="top"><a href="#set_host-2">set_host/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_param-3">set_param/3</a></td><td>set paramter of the URI.</td></tr><tr><td valign="top"><a href="#set_port-2">set_port/2</a></td><td>Set port number.</td></tr><tr><td valign="top"><a href="#set_raw_headers-2">set_raw_headers/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_user-2">set_user/2</a></td><td>Set user part of SIP URI.</td></tr><tr><td valign="top"><a href="#user-1">user/1</a></td><td>Get user part of SIP URI.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Uri::<a href="#type-uri">uri()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Uri::<a href="#type-uri">uri()</a>) -&gt; binary()
</code></pre>
<br />

<a name="assemble_scheme-1"></a>

### assemble_scheme/1 ###

`assemble_scheme(X1) -> any()`

<a name="clear_not_allowed_parts-2"></a>

### clear_not_allowed_parts/2 ###

<pre><code>
clear_not_allowed_parts(Type, Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>

<ul class="definitions"><li><code>Type = ruri | record_route</code></li></ul>

<a name="clear_params-1"></a>

### clear_params/1 ###

<pre><code>
clear_params(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="clear_transport-1"></a>

### clear_transport/1 ###

<pre><code>
clear_transport(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="data-1"></a>

### data/1 ###

<pre><code>
data(Uri::<a href="#type-uri">uri()</a>) -&gt; binary()
</code></pre>
<br />

Get data of the URI (everything after scheme).
Example:

```
     <<"+16505550505">> = ersip_uri:data(ersip_uri:make(<<"tel:+16505550505">>)).
     <<"a@b">> = ersip_uri:data(ersip_uri:make(<<"sip:a@b">>)).
```

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Part::<a href="#type-uri_part_name">uri_part_name()</a> | [<a href="#type-uri_part_name">uri_part_name()</a>], URI::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri_part">uri_part()</a> | [<a href="#type-uri_part">uri_part()</a>]
</code></pre>
<br />

Get URI part by identify. This function is depricated and will
be removed eventually.

<a name="host-1"></a>

### host/1 ###

<pre><code>
host(Uri::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="ersip_host.md#type-host">ersip_host:host()</a>
</code></pre>
<br />

Get host part of SIP URI.
Raises error if URI is not SIP(S) URI.

<a name="host_bin-1"></a>

### host_bin/1 ###

<pre><code>
host_bin(Uri::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; binary()
</code></pre>
<br />

Get host part of SIP URI in binary representation.
Raises error if URI is not SIP(S) URI.
Example:

```
     <<"atlanta.com">> = ersip_uri:host_bin(ersip_uri:make(<<"sip:alice@atlanta.com">>)).
     <<"biloxi.com">> = ersip_uri:host_bin(ersip_uri:make(<<"sips:bob@biloxi.com">>)).
     <<"127.0.0.1">> = ersip_uri:host_bin(ersip_uri:make(<<"sip:127.0.0.1">>)).
     ersip_uri:host_bin(ersip_uri:make(<<"tel:+16505550505">>)). % raises error
```

<a name="is_sip-1"></a>

### is_sip/1 ###

<pre><code>
is_sip(Uri::<a href="#type-uri">uri()</a>) -&gt; boolean()
</code></pre>
<br />

Returns true if URI is SIP or SIPS URI.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(PartsOrBin::[<a href="#type-uri_part">uri_part()</a>] | binary()) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="make_key-1"></a>

### make_key/1 ###

<pre><code>
make_key(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Make URI comparable with =:= erlang operator.  This means that
if make_key(UriA) =:= make_key(UriB) then they equal by RFC3261 19.1.4 URI Comparison.

<a name="params-1"></a>

### params/1 ###

<pre><code>
params(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri_params">uri_params()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Binary::binary()) -&gt; {ok, <a href="#type-uri">uri()</a>} | {error, {einval, atom()}}
</code></pre>
<br />

Parse URI from the binary

```
  SIP-URI          =  "sip:" [userinfo] hostport
                      uri-parameters [headers]
  SIPS-URI         =  "sips:" [userinfo] hostport
                      uri-parameters [headers]
```

<a name="port-1"></a>

### port/1 ###

<pre><code>
port(Uri::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; undefined | <a href="inet.md#type-port_number">inet:port_number()</a>
</code></pre>
<br />

Get port number of 'undefined'.
Raises error if URI is not SIP(S) URI.

```
     undefined = ersip_uri:port(ersip_uri:make(<<"sip:alice@atlanta.com">>)).
     5060 = ersip_uri:port(ersip_uri:make(<<"sip:alice@atlanta.com:5060">>)).
     ersip_uri:port(ersip_uri:make(<<"tel:+16505550505">>)). %% raises error
```

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Get raw value (in plain erlang types) of the uri.

<a name="raw_headers-1"></a>

### raw_headers/1 ###

<pre><code>
raw_headers(Uri::<a href="#type-uri">uri()</a>) -&gt; [{binary(), binary()}]
</code></pre>
<br />

<a name="raw_params-1"></a>

### raw_params/1 ###

<pre><code>
raw_params(Uri::<a href="#type-uri">uri()</a>) -&gt; [{binary(), binary()} | binary()]
</code></pre>
<br />

<a name="rebuild_header_values-1"></a>

### rebuild_header_values/1 ###

<pre><code>
rebuild_header_values(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="scheme-1"></a>

### scheme/1 ###

<pre><code>
scheme(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-scheme">scheme()</a>
</code></pre>
<br />

Scheme of the URI.

<a name="scheme_bin-1"></a>

### scheme_bin/1 ###

<pre><code>
scheme_bin(Uri::<a href="#type-uri">uri()</a>) -&gt; binary()
</code></pre>
<br />

URI scheme in binary form.

<a name="set_host-2"></a>

### set_host/2 ###

<pre><code>
set_host(H::<a href="ersip_host.md#type-host">ersip_host:host()</a>, Uri::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

<a name="set_param-3"></a>

### set_param/3 ###

<pre><code>
set_param(ParamName::<a href="#type-uri_param_name">uri_param_name()</a>, Value::term(), Uri::<a href="#type-uri">uri()</a> | <a href="#type-sip_uri_data">sip_uri_data()</a>) -&gt; <a href="#type-uri">uri()</a> | <a href="#type-sip_uri_data">sip_uri_data()</a>
</code></pre>
<br />

set paramter of the URI

<a name="set_port-2"></a>

### set_port/2 ###

<pre><code>
set_port(P::undefined | <a href="inet.md#type-port_number">inet:port_number()</a>, Uri::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

Set port number.
Raises error if URI is not SIP(S) URI.

<a name="set_raw_headers-2"></a>

### set_raw_headers/2 ###

<pre><code>
set_raw_headers(Headers::[{binary(), binary()}], Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="set_user-2"></a>

### set_user/2 ###

<pre><code>
set_user(NewUser::binary(), Uri::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

Set user part of SIP URI.
Raises error if URI is not SIP(S) URI.

<a name="user-1"></a>

### user/1 ###

<pre><code>
user(Uri::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; binary() | undefined
</code></pre>
<br />

Get user part of SIP URI.
Raises error if URI is not SIP(S) URI.
Example:

```
     <<"alice">> = ersip_uri:user(ersip_uri:make(<<"sip:alice@atlanta.com">>)).
     <<"bob">> = ersip_uri:user(ersip_uri:make(<<"sips:bob@biloxi.com">>)).
     undefined = ersip_uri:user(ersip_uri:make(<<"sip:biloxi.com">>)).
     ersip_uri:user(ersip_uri:make(<<"tel:+16505550505">>)). % raises error
```

