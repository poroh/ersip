

# Module ersip_authinfo #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Authoriztion info.

__See also:__ [for more information
](ersip_hdr_auth.md).

<a name="description"></a>

## Description ##

This is common module to process single value of following headers:
- WWW-Authenticate
- Authorization
- Proxy-Authenticate
- Proxy-Authorization

Note that more than one Authorization header maybe present is SIP
header.
<a name="types"></a>

## Data Types ##




### <a name="type-authinfo">authinfo()</a> ###


<pre><code>
authinfo() = #authinfo{type = binary(), params = <a href="ersip_parser_aux.md#type-gen_param_list">ersip_parser_aux:gen_param_list()</a>}
</code></pre>




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = {invalid_authinfo, {garbage_at_the_end, binary()}} | {invalid_authinfo, term()}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-authinfo">authinfo()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = {Type::binary(), [{binary(), binary()}]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Assemble authorization info into iolist.</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Assemble authorization info into binary.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Create AuthInfo from binary or from raw value.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse header from binary or from ersip_hdr header.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Get raw value (in plain erlang types) of the header.</td></tr><tr><td valign="top"><a href="#raw_param-2">raw_param/2</a></td><td>Get raw value of the parameter from authorization.</td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td>Type of the authorization info in lower case.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Authinfo::<a href="#type-authinfo">authinfo()</a>) -&gt; iolist()
</code></pre>
<br />

Assemble authorization info into iolist.

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Authinfo::<a href="#type-authinfo">authinfo()</a>) -&gt; binary()
</code></pre>
<br />

Assemble authorization info into binary.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary() | <a href="#type-raw">raw()</a>) -&gt; <a href="#type-authinfo">authinfo()</a>
</code></pre>
<br />

Create AuthInfo from binary or from raw value.
If parameter is not wellformed header than function raises error.
Example:

```
    AuthInfo = ersip_authinfo:make(<<"Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\", nonce=dcd98b7102dd2f0e8b11d0f600bfb0c093">>),
    AuthInfo = ersip_authinfo:make({<<"Digest">>, [{<<"realm">>, <<"testrealm@host.com">>},
                                                   {<<"qop">>, <<"auth,auth-int">>},
                                                   {<<"nonce">>, <<"dcd98b7102dd2f0e8b11d0f600bfb0c093">>}]}).
```

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

returns: {ok, authinfo()} or {error, {invalid_authinfo, term()}}.
Example:

```
    {ok, AuthInfo} = ersip_authinfo:parse(<<"Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\", nonce=dcd98b7102dd2f0e8b11d0f600bfb0c093">>),
    {error, _} = ersip_authinfo:parse(<<"@">>).
```

Parse header from binary or from ersip_hdr header.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Authinfo::<a href="#type-authinfo">authinfo()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Get raw value (in plain erlang types) of the header.
Plain format here is `{Type :: binary(), [{LowerParameterName :: binary(), UnquotedValue : binary()}]}`
Example:

```
    AuthInfo = ersip_authinfo:make(<<"Digest realm=\"testrealm@host.com\", QOP=\"auth,auth-int\", nonce=dcd98b7102dd2f0e8b11d0f600bfb0c093">>),
    {<<"digest">>,
     [{<<"realm">>, <<"testrealm@host.com">>},
      {<<"qop">>, <<"auth,auth-int">>},
      {<<"nonce">>, <<"dcd98b7102dd2f0e8b11d0f600bfb0c093">>}]}
      = ersip_authinfo:raw(AuthInfo).
```

<a name="raw_param-2"></a>

### raw_param/2 ###

<pre><code>
raw_param(Name::binary(), Authinfo::<a href="#type-authinfo">authinfo()</a>) -&gt; {ok, binary()} | undefined
</code></pre>
<br />

Get raw value of the parameter from authorization.
Note that value is returned as-is. If it was quoted string it is returned as quoted string.
If parameter is not found then undefined atom is returned.
Example:

```
    AuthInfo = ersip_authinfo:make(<<"Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\"">>),
    {ok, <<"\"auth,auth-int\"">>} = ersip_authinfo:raw_param(<<"QOP">>, AuthInfo).
    undefined = ersip_authinfo:raw_param(<<"cnonce">>, AuthInfo).
```

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(Authinfo::<a href="#type-authinfo">authinfo()</a>) -&gt; binary()
</code></pre>
<br />

Type of the authorization info in lower case.
Example:

```
    AuthInfo = ersip_authinfo:make(<<"Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\"">>),
    <<"digest">> = ersip_authinfo:type(AuthInfo).
```

