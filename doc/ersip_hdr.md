

# Module ersip_hdr #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

SIP raw header.

<a name="description"></a>

## Description ##
This module used for low-level generic header.

<a name="types"></a>

## Data Types ##




### <a name="type-header">header()</a> ###


<pre><code>
header() = #header{name = binary(), key = <a href="#type-header_key">header_key()</a>, values = [<a href="#type-value">value()</a>]}
</code></pre>




### <a name="type-header_key">header_key()</a> ###


<pre><code>
header_key() = <a href="ersip_hnames.md#type-header_key">ersip_hnames:header_key()</a>
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = iolist() | binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_topmost-2">add_topmost/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_value-2">add_value/2</a></td><td>Append value to list of values.</td></tr><tr><td valign="top"><a href="#add_values-2">add_values/2</a></td><td>Append list of values to headr's list of values.</td></tr><tr><td valign="top"><a href="#as_integer-1">as_integer/1</a></td><td>Get integer value from the header.</td></tr><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_key-1">make_key/1</a></td><td>Create key by the header name or get header from header
itself.</td></tr><tr><td valign="top"><a href="#name-1">name/1</a></td><td>User-readable non-comparable header name.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Create new headers.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#raw_values-1">raw_values/1</a></td><td>Return raw values of the header.</td></tr><tr><td valign="top"><a href="#replace_topmost-2">replace_topmost/2</a></td><td></td></tr><tr><td valign="top"><a href="#serialize_rev_iolist-2">serialize_rev_iolist/2</a></td><td>serialize header values in reverse iolist If Acc is not empty
then also adds CR LF before adding header.</td></tr><tr><td valign="top"><a href="#take_topmost-1">take_topmost/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_topmost-2"></a>

### add_topmost/2 ###

<pre><code>
add_topmost(Value::<a href="#type-value">value()</a>, Header::<a href="#type-header">header()</a>) -&gt; <a href="#type-header">header()</a>
</code></pre>
<br />

<a name="add_value-2"></a>

### add_value/2 ###

<pre><code>
add_value(Value::<a href="#type-value">value()</a>, Header::<a href="#type-header">header()</a>) -&gt; <a href="#type-header">header()</a>
</code></pre>
<br />

Append value to list of values.

<a name="add_values-2"></a>

### add_values/2 ###

<pre><code>
add_values(Value::<a href="#type-value">value()</a>, Header::<a href="#type-header">header()</a>) -&gt; <a href="#type-header">header()</a>
</code></pre>
<br />

Append list of values to headr's list of values.

<a name="as_integer-1"></a>

### as_integer/1 ###

<pre><code>
as_integer(Header::<a href="#type-header">header()</a>) -&gt; {ok, integer()} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Error = invalid_integer | multiple_values | no_header</code></li></ul>

Get integer value from the header.

<a name="is_empty-1"></a>

### is_empty/1 ###

<pre><code>
is_empty(Header::<a href="#type-header">header()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="make_key-1"></a>

### make_key/1 ###

<pre><code>
make_key(Header::<a href="ersip_hnames.md#type-name_forms">ersip_hnames:name_forms()</a> | <a href="#type-header">header()</a>) -&gt; <a href="#type-header_key">header_key()</a>
</code></pre>
<br />

Create key by the header name or get header from header
itself.

<a name="name-1"></a>

### name/1 ###

<pre><code>
name(Header::<a href="#type-header">header()</a>) -&gt; binary()
</code></pre>
<br />

User-readable non-comparable header name.

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Name::<a href="ersip_hnames.md#type-name_forms">ersip_hnames:name_forms()</a>) -&gt; <a href="#type-header">header()</a>
</code></pre>
<br />

Create new headers.

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Name::binary(), Key::<a href="#type-header_key">header_key()</a>) -&gt; <a href="#type-header">header()</a>
</code></pre>
<br />

<a name="raw_values-1"></a>

### raw_values/1 ###

<pre><code>
raw_values(Header::<a href="#type-header">header()</a>) -&gt; [<a href="#type-value">value()</a>]
</code></pre>
<br />

Return raw values of the header.

<a name="replace_topmost-2"></a>

### replace_topmost/2 ###

<pre><code>
replace_topmost(Value::<a href="#type-value">value()</a>, Header::<a href="#type-header">header()</a>) -&gt; <a href="#type-header">header()</a>
</code></pre>
<br />

<a name="serialize_rev_iolist-2"></a>

### serialize_rev_iolist/2 ###

<pre><code>
serialize_rev_iolist(Header::<a href="#type-header">header()</a>, Acc::list()) -&gt; list()
</code></pre>
<br />

serialize header values in reverse iolist If Acc is not empty
then also adds CR LF before adding header. If header has more than
one value then multiple headers are added separated by CR LF.

<a name="take_topmost-1"></a>

### take_topmost/1 ###

<pre><code>
take_topmost(Header::<a href="#type-header">header()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, <a href="#type-value">value()</a>, <a href="#type-header">header()</a>} | {error, no_header}</code></li></ul>

