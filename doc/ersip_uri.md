

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




### <a name="type-known_param">known_param()</a> ###


<pre><code>
known_param() = transport | lr | ttl | user | maddr
</code></pre>




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = {invalid_scheme, binary()} | {invalid_sip_uri, <a href="#type-sip_uri_parse_error">sip_uri_parse_error()</a>}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-uri">uri()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = #{scheme =&gt; binary(), data =&gt; binary(), sip =&gt; <a href="#type-sip_uri_raw">sip_uri_raw()</a>}
</code></pre>




### <a name="type-scheme">scheme()</a> ###


<pre><code>
scheme() = {scheme, sip | sips | binary()}
</code></pre>




### <a name="type-sip_uri_data">sip_uri_data()</a> ###


<pre><code>
sip_uri_data() = #sip_uri_data{user = undefined | {user, binary()}, host = <a href="ersip_host.md#type-host">ersip_host:host()</a>, host_orig = binary() | undefined, port = undefined | <a href="inet.md#type-port_number">inet:port_number()</a>, params = <a href="#type-uri_params">uri_params()</a>, headers = <a href="#type-uri_headers">uri_headers()</a>}
</code></pre>




### <a name="type-sip_uri_parse_error">sip_uri_parse_error()</a> ###


<pre><code>
sip_uri_parse_error() = {invalid_host, <a href="ersip_host.md#type-parse_error">ersip_host:parse_error()</a> | {garbage_at_the_end, binary()}} | {invalid_ipv6_reference, binary()} | {invalid_port, binary()}
</code></pre>




### <a name="type-sip_uri_raw">sip_uri_raw()</a> ###


<pre><code>
sip_uri_raw() = #{host =&gt; binary(), user =&gt; binary(), port =&gt; <a href="inet.md#type-port_number">inet:port_number()</a>, params =&gt; <a href="#type-key_value_list">key_value_list()</a>, headers =&gt; <a href="#type-key_value_list">key_value_list()</a>}
</code></pre>




### <a name="type-ttl_param">ttl_param()</a> ###


<pre><code>
ttl_param() = 0..255
</code></pre>




### <a name="type-uri">uri()</a> ###


<pre><code>
uri() = #uri{scheme = <a href="#type-scheme">scheme()</a>, data = <a href="#type-uri_data">uri_data()</a>}
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
uri_param_name() = <a href="#type-known_param">known_param()</a> | binary()
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




### <a name="type-user_param">user_param()</a> ###


<pre><code>
user_param() = phone | ip | binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Assemble URI to iolist.</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Assemble URI to binary.</td></tr><tr><td valign="top"><a href="#assemble_scheme-1">assemble_scheme/1</a></td><td></td></tr><tr><td valign="top"><a href="#clear_gen_param-2">clear_gen_param/2</a></td><td>Clear generic parameter of URI.</td></tr><tr><td valign="top"><a href="#clear_maddr-1">clear_maddr/1</a></td><td>Remove maddr parameter from URI.</td></tr><tr><td valign="top"><a href="#clear_not_allowed_parts-2">clear_not_allowed_parts/2</a></td><td>Clear not allowed par of the URI in context.</td></tr><tr><td valign="top"><a href="#clear_params-1">clear_params/1</a></td><td>Clear all URI parameters.</td></tr><tr><td valign="top"><a href="#clear_transport-1">clear_transport/1</a></td><td>Remove transport parameter from URI.</td></tr><tr><td valign="top"><a href="#clear_ttl-1">clear_ttl/1</a></td><td>Clear TTL parameter from URI.</td></tr><tr><td valign="top"><a href="#clear_user_param-1">clear_user_param/1</a></td><td>Clear user parameter from URI.</td></tr><tr><td valign="top"><a href="#data-1">data/1</a></td><td>Get data of the URI (everything after scheme).</td></tr><tr><td valign="top"><a href="#gen_param-2">gen_param/2</a></td><td>Get generic parameter of the URI.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Get URI part by identify.</td></tr><tr><td valign="top"><a href="#host-1">host/1</a></td><td>Get host part of SIP URI.</td></tr><tr><td valign="top"><a href="#host_bin-1">host_bin/1</a></td><td>Get host part of SIP URI in binary representation.</td></tr><tr><td valign="top"><a href="#is_sip-1">is_sip/1</a></td><td>Returns true if URI is SIP or SIPS URI.</td></tr><tr><td valign="top"><a href="#loose_router-1">loose_router/1</a></td><td>Checks if URI has loose router parameter (lr).</td></tr><tr><td valign="top"><a href="#maddr-1">maddr/1</a></td><td>Return maddr parameter value or undefined.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Create URI from binary, raw representation and deprecated from URI parts.</td></tr><tr><td valign="top"><a href="#make_key-1">make_key/1</a></td><td>Make URI comparable with =:= erlang operator.</td></tr><tr><td valign="top"><a href="#params-1">params/1</a></td><td>Get URI params.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse URI from the binary
<pre>  SIP-URI          =  "sip:" [userinfo] hostport
                      uri-parameters [headers]
  SIPS-URI         =  "sips:" [userinfo] hostport
                      uri-parameters [headers]</pre></td></tr><tr><td valign="top"><a href="#port-1">port/1</a></td><td>Get port number of 'undefined'.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Get raw value (in plain erlang types) of the uri.</td></tr><tr><td valign="top"><a href="#raw_headers-1">raw_headers/1</a></td><td>Get raw URI headers as list.</td></tr><tr><td valign="top"><a href="#raw_params-1">raw_params/1</a></td><td>Get raw URI params as list.</td></tr><tr><td valign="top"><a href="#rebuild_header_values-1">rebuild_header_values/1</a></td><td>Unquote and quote again headers.</td></tr><tr><td valign="top"><a href="#scheme-1">scheme/1</a></td><td>Scheme of the URI.</td></tr><tr><td valign="top"><a href="#scheme_bin-1">scheme_bin/1</a></td><td>URI scheme in binary form.</td></tr><tr><td valign="top"><a href="#set_gen_param-3">set_gen_param/3</a></td><td>Set generic parameter of URI.</td></tr><tr><td valign="top"><a href="#set_host-2">set_host/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_loose_router-2">set_loose_router/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_maddr-2">set_maddr/2</a></td><td>Set maddr parameter value.</td></tr><tr><td valign="top"><a href="#set_param-3">set_param/3</a></td><td>(<em>Deprecated</em>.) Set parameter of the URI.</td></tr><tr><td valign="top"><a href="#set_port-2">set_port/2</a></td><td>Set port number.</td></tr><tr><td valign="top"><a href="#set_raw_headers-2">set_raw_headers/2</a></td><td>Set raw URI headers from list.</td></tr><tr><td valign="top"><a href="#set_transport-2">set_transport/2</a></td><td>Set transport to URI.</td></tr><tr><td valign="top"><a href="#set_ttl-2">set_ttl/2</a></td><td>Set ttl parameter of URI.</td></tr><tr><td valign="top"><a href="#set_user-2">set_user/2</a></td><td>Set user part of SIP URI.</td></tr><tr><td valign="top"><a href="#set_user_param-2">set_user_param/2</a></td><td>Set user parameter of URI.</td></tr><tr><td valign="top"><a href="#transport-1">transport/1</a></td><td>Get transport from URI.</td></tr><tr><td valign="top"><a href="#ttl-1">ttl/1</a></td><td>Get ttl parameter of URI.</td></tr><tr><td valign="top"><a href="#user-1">user/1</a></td><td>Get user part of SIP URI.</td></tr><tr><td valign="top"><a href="#user_param-1">user_param/1</a></td><td>Get user parameter of URI (Ex: ;user=ip or ;user=phone).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Uri::<a href="#type-uri">uri()</a>) -&gt; iolist()
</code></pre>
<br />

Assemble URI to iolist.

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Uri::<a href="#type-uri">uri()</a>) -&gt; binary()
</code></pre>
<br />

Assemble URI to binary.

<a name="assemble_scheme-1"></a>

### assemble_scheme/1 ###

`assemble_scheme(X1) -> any()`

<a name="clear_gen_param-2"></a>

### clear_gen_param/2 ###

<pre><code>
clear_gen_param(Name::binary(), Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Clear generic parameter of URI.
Raises error if URI is not SIP(S) URI.

<a name="clear_maddr-1"></a>

### clear_maddr/1 ###

<pre><code>
clear_maddr(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Remove maddr parameter from URI.
Raises error if URI is not SIP(S) URI.

<a name="clear_not_allowed_parts-2"></a>

### clear_not_allowed_parts/2 ###

<pre><code>
clear_not_allowed_parts(Type, Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>

<ul class="definitions"><li><code>Type = ruri | record_route</code></li></ul>

Clear not allowed par of the URI in context.

```
                                                        dialog
                                            reg./redir. Contact/
                default  Req.-URI  To  From  Contact   R-R/Route  external
  user          --          o      o    o       o          o         o
  password      --          o      o    o       o          o         o
  host          --          m      m    m       m          m         m
  port          (1)         o      -    -       o          o         o
  user-param    ip          o      o    o       o          o         o
  method        INVITE      -      -    -       -          -         o
  maddr-param   --          o      -    -       o          o         o
  ttl-param     1           o      -    -       o          -         o
  transp.-param (2)         o      -    -       o          o         o
  lr-param      --          o      -    -       -          o         o
  other-param   --          o      o    o       o          o         o
  headers       --          -      -    -       o          -         o
```

<a name="clear_params-1"></a>

### clear_params/1 ###

<pre><code>
clear_params(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Clear all URI parameters.

<a name="clear_transport-1"></a>

### clear_transport/1 ###

<pre><code>
clear_transport(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Remove transport parameter from URI.
Raises error if URI is not SIP(S) URI.

<a name="clear_ttl-1"></a>

### clear_ttl/1 ###

<pre><code>
clear_ttl(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Clear TTL parameter from URI.
Raises error if URI is not SIP(S) URI.

<a name="clear_user_param-1"></a>

### clear_user_param/1 ###

<pre><code>
clear_user_param(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Clear user parameter from URI.
Raises error if URI is not SIP(S) URI.

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

<a name="gen_param-2"></a>

### gen_param/2 ###

<pre><code>
gen_param(Name::binary(), Uri::<a href="#type-uri">uri()</a>) -&gt; binary() | undefined
</code></pre>
<br />

Get generic parameter of the URI.
Raises error if URI is not SIP(S) URI.
This function also can be used to get known parameters in generic form
Example:

```
    <<"11">> = ersip_uri:gen_param(<<"ttl">>, ersip_uri:make(<<"sip:b;ttl=11">>)).
    true = ersip_uri:gen_param(<<"lr">>, ersip_uri:make(<<"sip:b;lr">>)).
    undefined = ersip_uri:gen_param(<<"lr">>, ersip_uri:make(<<"sip:b">>)).
```

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Part::<a href="#type-uri_part_name">uri_part_name()</a> | [<a href="#type-uri_part_name">uri_part_name()</a>], URI::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri_part">uri_part()</a> | [<a href="#type-uri_part">uri_part()</a>]
</code></pre>
<br />

Get URI part by identify. This function is deprecated and will
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

<a name="loose_router-1"></a>

### loose_router/1 ###

<pre><code>
loose_router(Uri::<a href="#type-uri">uri()</a>) -&gt; boolean()
</code></pre>
<br />

Checks if URI has loose router parameter (lr).
Raises error if URI is not SIP(S) URI.
Example:

```
    true  = ersip_uri:loose_route(ersip_uri:make(<<"sip:host;lr">>)).
    false = ersip_uri:loose_route(ersip_uri:make(<<"sip:host">>)).
```

<a name="maddr-1"></a>

### maddr/1 ###

<pre><code>
maddr(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="ersip_host.md#type-host">ersip_host:host()</a> | undefined
</code></pre>
<br />

Return maddr parameter value or undefined.
Raises error if URI is not SIP(S) URI.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary() | <a href="#type-raw">raw()</a> | [<a href="#type-uri_part">uri_part()</a>]) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Create URI from binary, raw representation and deprecated from URI parts.
Note that creation from URI parts are deprecated and will be
removed in future releases.
Raises error if URI cannot be constracted from this data (has invalid syntax).
Examples:

```
    SIPURI = ersip_uri:make(<<"sip:a@b">>),
    SIPURI = ersip_uri:make(#{scheme => <<"sip">>, data => <<"a@b">>}),
    TelURI = ersip_uri:make(<<"tel:+16505550505">>),
    TelURI = ersip_uri:make(#{scheme => <<"tel">>, data => <<"+16505550505">>}).
```

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

Get URI params.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Binary::binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
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

Get raw URI headers as list.

<a name="raw_params-1"></a>

### raw_params/1 ###

<pre><code>
raw_params(Uri::<a href="#type-uri">uri()</a>) -&gt; [{binary(), binary()} | binary()]
</code></pre>
<br />

Get raw URI params as list.

<a name="rebuild_header_values-1"></a>

### rebuild_header_values/1 ###

<pre><code>
rebuild_header_values(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Unquote and quote again headers.

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

<a name="set_gen_param-3"></a>

### set_gen_param/3 ###

<pre><code>
set_gen_param(Name::binary(), Value::binary(), Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Set generic parameter of URI.
Raises error if URI is not SIP(S) URI.

<a name="set_host-2"></a>

### set_host/2 ###

<pre><code>
set_host(H::<a href="ersip_host.md#type-host">ersip_host:host()</a>, Uri::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

<a name="set_loose_router-2"></a>

### set_loose_router/2 ###

<pre><code>
set_loose_router(X1::boolean(), Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="set_maddr-2"></a>

### set_maddr/2 ###

<pre><code>
set_maddr(Host::<a href="ersip_host.md#type-host">ersip_host:host()</a>, Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Set maddr parameter value.
Raises error if URI is not SIP(S) URI or if first parameter is not
host.

<a name="set_param-3"></a>

### set_param/3 ###

<pre><code>
set_param(ParamName::<a href="#type-uri_param_name">uri_param_name()</a>, Value::term(), Uri::<a href="#type-uri">uri()</a> | <a href="#type-sip_uri_data">sip_uri_data()</a>) -&gt; <a href="#type-uri">uri()</a> | <a href="#type-sip_uri_data">sip_uri_data()</a>
</code></pre>
<br />

__This function is deprecated:__ 
This function is deprecated. Please use set_gen_param for generic
form and set_transport, set_ttl, set_... for known params.

Set parameter of the URI

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

Set raw URI headers from list.

<a name="set_transport-2"></a>

### set_transport/2 ###

<pre><code>
set_transport(Transport::<a href="ersip_transport.md#type-transport">ersip_transport:transport()</a>, Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Set transport to URI.
Raises error if URI is not SIP(S) URI.

<a name="set_ttl-2"></a>

### set_ttl/2 ###

<pre><code>
set_ttl(TTL::<a href="#type-ttl_param">ttl_param()</a>, Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Set ttl parameter of URI.
Raises error if URI is not SIP(S) URI.

<a name="set_user-2"></a>

### set_user/2 ###

<pre><code>
set_user(NewUser::binary(), Uri::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>
</code></pre>
<br />

Set user part of SIP URI.
Raises error if URI is not SIP(S) URI.

<a name="set_user_param-2"></a>

### set_user_param/2 ###

<pre><code>
set_user_param(UserParam::<a href="#type-user_param">user_param()</a>, Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Set user parameter of URI.
Raises error if URI is not SIP(S) URI.

<a name="transport-1"></a>

### transport/1 ###

<pre><code>
transport(Uri::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; undefined | <a href="ersip_transport.md#type-transport">ersip_transport:transport()</a>
</code></pre>
<br />

Get transport from URI.
Raises error if URI is not SIP(S) URI.

<a name="ttl-1"></a>

### ttl/1 ###

<pre><code>
ttl(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-ttl_param">ttl_param()</a> | undefined
</code></pre>
<br />

Get ttl parameter of URI.
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

<a name="user_param-1"></a>

### user_param/1 ###

<pre><code>
user_param(Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-user_param">user_param()</a> | undefined
</code></pre>
<br />

Get user parameter of URI (Ex: ;user=ip or ;user=phone).
Raises error if URI is not SIP(S) URI.

