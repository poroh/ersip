

# Module ersip_msg #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-header_name">header_name()</a> ###


<pre><code>
header_name() = <a href="ersip_hnames.md#type-name_forms">ersip_hnames:name_forms()</a>
</code></pre>




### <a name="type-item">item()</a> ###


<pre><code>
item() = type | status | reason | method | ruri | body | <a href="#type-header_name">header_name()</a>
</code></pre>




### <a name="type-message">message()</a> ###


<pre><code>
message() = #message{type = {request, <a href="#type-method">method()</a> | undefined, binary() | undefined} | {response, 100..699 | undefined, binary() | undefined} | undefined, headers = #{<a href="ersip_hnames.md#type-header_key">ersip_hnames:header_key()</a> =&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>}, body = iolist(), source = undefined | <a href="ersip_source.md#type-source">ersip_source:source()</a>}
</code></pre>




### <a name="type-method">method()</a> ###


<pre><code>
method() = <a href="ersip_method.md#type-method">ersip_method:method()</a>
</code></pre>




### <a name="type-type">type()</a> ###


<pre><code>
type() = request | response
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-3">add/3</a></td><td></td></tr><tr><td valign="top"><a href="#clear_headers-1">clear_headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#del_header-2">del_header/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_headers-1">get_headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#header_keys-1">header_keys/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#serialize-1">serialize/1</a></td><td></td></tr><tr><td valign="top"><a href="#serialize_bin-1">serialize_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td></td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>Set message part to specified value.</td></tr><tr><td valign="top"><a href="#set_header-2">set_header/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_source-2">set_source/2</a></td><td></td></tr><tr><td valign="top"><a href="#source-1">source/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-3"></a>

### add/3 ###

<pre><code>
add(Name::binary(), Value::binary(), Message::<a href="#type-message">message()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

<a name="clear_headers-1"></a>

### clear_headers/1 ###

<pre><code>
clear_headers(Message::<a href="#type-message">message()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

<a name="del_header-2"></a>

### del_header/2 ###

<pre><code>
del_header(HeaderName::<a href="ersip_hnames.md#type-name_forms">ersip_hnames:name_forms()</a> | <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>, Message::<a href="#type-message">message()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(ItemList::<a href="#type-item">item()</a>, Message::<a href="#type-message">message()</a>) -&gt; term()
</code></pre>
<br />

<a name="get_headers-1"></a>

### get_headers/1 ###

<pre><code>
get_headers(Message::<a href="#type-message">message()</a>) -&gt; [<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>]
</code></pre>
<br />

<a name="header_keys-1"></a>

### header_keys/1 ###

<pre><code>
header_keys(Message::<a href="#type-message">message()</a>) -&gt; [<a href="ersip_hnames.md#type-header_key">ersip_hnames:header_key()</a>]
</code></pre>
<br />

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

<a name="serialize-1"></a>

### serialize/1 ###

<pre><code>
serialize(Message::<a href="#type-message">message()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="serialize_bin-1"></a>

### serialize_bin/1 ###

<pre><code>
serialize_bin(Message::<a href="#type-message">message()</a>) -&gt; binary()
</code></pre>
<br />

<a name="set-2"></a>

### set/2 ###

<pre><code>
set(List::[{<a href="#type-item">item()</a>, term()}], Message::<a href="#type-message">message()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

<a name="set-3"></a>

### set/3 ###

<pre><code>
set(HeaderName::<a href="#type-item">item()</a>, X::term(), Message::<a href="#type-message">message()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Set message part to specified value.

<a name="set_header-2"></a>

### set_header/2 ###

<pre><code>
set_header(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>, Message::<a href="#type-message">message()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

<a name="set_source-2"></a>

### set_source/2 ###

<pre><code>
set_source(Source::<a href="ersip_source.md#type-source">ersip_source:source()</a>, Message::<a href="#type-message">message()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

<a name="source-1"></a>

### source/1 ###

<pre><code>
source(Message::<a href="#type-message">message()</a>) -&gt; undefined | <a href="ersip_source.md#type-source">ersip_source:source()</a>
</code></pre>
<br />

