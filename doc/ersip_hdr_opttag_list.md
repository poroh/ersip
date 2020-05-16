

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

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#append-2">append/2</a></td><td></td></tr><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#intersect-2">intersect/2</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#subtract-2">subtract/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="append-2"></a>

### append/2 ###

<pre><code>
append(OptionTag::<a href="ersip_option_tag.md#type-option_tag">ersip_option_tag:option_tag()</a>, X2::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; <a href="#type-option_tag_list">option_tag_list()</a>
</code></pre>
<br />

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(OptionTagList::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), OptionTagList::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(OptionTagList::[<a href="ersip_option_tag.md#type-option_tag">ersip_option_tag:option_tag()</a>]) -&gt; <a href="#type-option_tag_list">option_tag_list()</a>
</code></pre>
<br />

<a name="intersect-2"></a>

### intersect/2 ###

<pre><code>
intersect(X1::<a href="#type-option_tag_list">option_tag_list()</a>, X2::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; <a href="#type-option_tag_list">option_tag_list()</a>
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-option_tag_list">option_tag_list()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, <a href="#type-option_tag_list">option_tag_list()</a>} | {error, Error}</code></li><li><code>Error = no_header | {invalid_option_tag_list, term()}</code></li></ul>

<a name="subtract-2"></a>

### subtract/2 ###

<pre><code>
subtract(X1::<a href="#type-option_tag_list">option_tag_list()</a>, X2::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; <a href="#type-option_tag_list">option_tag_list()</a>
</code></pre>
<br />

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(X1::<a href="#type-option_tag_list">option_tag_list()</a>) -&gt; [<a href="ersip_option_tag.md#type-option_tag">ersip_option_tag:option_tag()</a>]
</code></pre>
<br />

