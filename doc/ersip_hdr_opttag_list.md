

# Module ersip_hdr_opttag_list #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-option_tag_list">option_tag_list()</a> ###


<pre><code>
option_tag_list() = {option_tag_list, <a href="gb_sets.md#type-set">gb_sets:set</a>(<a href="ersip_option_tag.md#type-option_tag">ersip_option_tag:option_tag()</a>)}
</code></pre>




### <a name="type-parse_error">parse_error()</a> ###


<pre><code>
parse_error() = no_header | {invalid_option_tag_list, term()}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-option_tag_list">option_tag_list()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = [binary()]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#append-2">append/2</a></td><td>Append option tag to option tags.</td></tr><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Assemble options tags list to iolist.</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Assemble options tags list to binary.</td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td>Build raw SIP header from options tags list.</td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td>Create from list of option tags.</td></tr><tr><td valign="top"><a href="#intersect-2">intersect/2</a></td><td>Create intersection of sets of option tags.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Make option tags from binary() or raw representation.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse options tags from binary or raw SIP header.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Raw representation of option tags list.</td></tr><tr><td valign="top"><a href="#subtract-2">subtract/2</a></td><td>Create subtractions of sets of option tags.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>Save to list of option tags.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="append-2"></a>

### append/2 ###

<pre><code>
append(OptionTag::<a href="ersip_option_tag.md#type-option_tag">ersip_option_tag:option_tag()</a>, X2::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; <a href="#type-option_tag_list">option_tag_list()</a>
</code></pre>
<br />

Append option tag to option tags.

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(OptionTagList::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; iolist()
</code></pre>
<br />

Assemble options tags list to iolist

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(OptionTagList::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; binary()
</code></pre>
<br />

Assemble options tags list to binary

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), OptionTagList::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

Build raw SIP header from options tags list.

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(OptionTagList::[<a href="ersip_option_tag.md#type-option_tag">ersip_option_tag:option_tag()</a>]) -&gt; <a href="#type-option_tag_list">option_tag_list()</a>
</code></pre>
<br />

Create from list of option tags.

<a name="intersect-2"></a>

### intersect/2 ###

<pre><code>
intersect(X1::<a href="#type-option_tag_list">option_tag_list()</a>, X2::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; <a href="#type-option_tag_list">option_tag_list()</a>
</code></pre>
<br />

Create intersection of sets of option tags.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary() | <a href="#type-raw">raw()</a>) -&gt; <a href="#type-option_tag_list">option_tag_list()</a>
</code></pre>
<br />

Make option tags from binary() or raw representation.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Binary::binary() | <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

Parse options tags from binary or raw SIP header.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(OTL::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Raw representation of option tags list.

<a name="subtract-2"></a>

### subtract/2 ###

<pre><code>
subtract(X1::<a href="#type-option_tag_list">option_tag_list()</a>, X2::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; <a href="#type-option_tag_list">option_tag_list()</a>
</code></pre>
<br />

Create subtractions of sets of option tags.

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(X1::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; [<a href="ersip_option_tag.md#type-option_tag">ersip_option_tag:option_tag()</a>]
</code></pre>
<br />

Save to list of option tags.

