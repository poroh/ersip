

# Module ersip_nameaddr #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-display_name">display_name()</a> ###


<pre><code>
display_name() = <a href="ersip_display_name.md#type-display_name">ersip_display_name:display_name()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-2">assemble/2</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_display_name-1">assemble_display_name/1</a></td><td>Assemble disloay name to iolist.</td></tr><tr><td valign="top"><a href="#assemble_display_name_bin-1">assemble_display_name_bin/1</a></td><td>Assemble disloay name to iolist.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-2"></a>

### assemble/2 ###

<pre><code>
assemble(DisplayName::<a href="#type-display_name">display_name()</a>, URI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="assemble_display_name-1"></a>

### assemble_display_name/1 ###

<pre><code>
assemble_display_name(DN::<a href="#type-display_name">display_name()</a>) -&gt; iolist()
</code></pre>
<br />

Assemble disloay name to iolist

Please use ersip_display_name:assemble/1.

<a name="assemble_display_name_bin-1"></a>

### assemble_display_name_bin/1 ###

<pre><code>
assemble_display_name_bin(DN::<a href="#type-display_name">display_name()</a>) -&gt; binary()
</code></pre>
<br />

Assemble disloay name to iolist

Please use ersip_display_name:assemble_bin/1.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(NameAddrBin::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>({<a href="#type-display_name">display_name()</a>, <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>})
</code></pre>
<br />

<a name="parse-2"></a>

### parse/2 ###

<pre><code>
parse(NameAddrBin::binary(), AddrSpecSeps::[binary()]) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>({<a href="#type-display_name">display_name()</a>, <a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>})
</code></pre>
<br />

