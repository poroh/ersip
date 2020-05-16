

# Module ersip_hdr_allow #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Allow Header Implementation.

<a name="description"></a>

## Description ##

<a name="types"></a>

## Data Types ##




### <a name="type-allow">allow()</a> ###


<pre><code>
allow() = {allow, <a href="ersip_method_set.md#type-set">ersip_method_set:set()</a>}
</code></pre>




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = no_allow | {invalid_allow, binary()}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-allow">allow()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = <a href="ersip_method_set.md#type-raw">ersip_method_set:raw()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Serialize header to iolist.</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Serialize the header to binary.</td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td>Create lowlevel ersip_hdr from Allow header.</td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td>Create Allow header from list of methods.</td></tr><tr><td valign="top"><a href="#from_method_set-1">from_method_set/1</a></td><td>Create Allow header from method set.</td></tr><tr><td valign="top"><a href="#has-2">has/2</a></td><td>Check if header has method.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Create Allow header from binary or from raw value.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse header from binary or from ersip_hdr header.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Get raw value (in plain erlang types) of the header.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>Get all methods from Allow header.</td></tr><tr><td valign="top"><a href="#to_method_set-1">to_method_set/1</a></td><td>Get method set from Allow header.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Allow::<a href="#type-allow">allow()</a>) -&gt; iolist()
</code></pre>
<br />

Serialize header to iolist.

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Allow::<a href="#type-allow">allow()</a>) -&gt; binary()
</code></pre>
<br />

Serialize the header to binary.

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), Allow::<a href="#type-allow">allow()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

Create lowlevel ersip_hdr from Allow header.

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(MethodList::[<a href="ersip_method.md#type-method">ersip_method:method()</a>]) -&gt; <a href="#type-allow">allow()</a>
</code></pre>
<br />

Create Allow header from list of methods.

<a name="from_method_set-1"></a>

### from_method_set/1 ###

<pre><code>
from_method_set(MethodSet::<a href="ersip_method_set.md#type-set">ersip_method_set:set()</a>) -&gt; <a href="#type-allow">allow()</a>
</code></pre>
<br />

Create Allow header from method set.

<a name="has-2"></a>

### has/2 ###

<pre><code>
has(M::<a href="ersip_method.md#type-method">ersip_method:method()</a>, X2::<a href="#type-allow">allow()</a>) -&gt; boolean()
</code></pre>
<br />

Check if header has method.
Example:

```
      Allow = ersip_hdr_allow:make(<<"INVITE, ACK, BYE, CANCEL, OPTIONS">>),
      true = ersip_hdr_allow:has(ersip_method:make(<<"INVITE">>), Allow).
```

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Value::binary() | <a href="#type-raw">raw()</a>) -&gt; <a href="#type-allow">allow()</a>
</code></pre>
<br />

Create Allow header from binary or from raw value.
Raise error if input is not well-formed Allow header or incorrect raw value.
Example:

```
    Allow = ersip_hdr_allow:make(<<"INVITE, ACK, BYE, CANCEL, OPTIONS">>).
    Allow = ersip_hdr_allow:make([<<"INVITE>>, <<"ACK">>, <<"BYE">>, <<"CANCEL">>, <<"OPTIONS">>]).
```

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(HeaderBin::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

Parse header from binary or from ersip_hdr header.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Allow::<a href="#type-allow">allow()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Get raw value (in plain erlang types) of the header.

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(X1::<a href="#type-allow">allow()</a>) -&gt; [<a href="ersip_method.md#type-method">ersip_method:method()</a>]
</code></pre>
<br />

Get all methods from Allow header.

<a name="to_method_set-1"></a>

### to_method_set/1 ###

<pre><code>
to_method_set(X1::<a href="#type-allow">allow()</a>) -&gt; <a href="ersip_method_set.md#type-set">ersip_method_set:set()</a>
</code></pre>
<br />

Get method set from Allow header.

