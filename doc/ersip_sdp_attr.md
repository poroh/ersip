

# Module ersip_sdp_attr #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-attr">attr()</a> ###


<pre><code>
attr() = <a href="#type-attr_name">attr_name()</a> | {<a href="#type-attr_name">attr_name()</a>, <a href="#type-attr_value">attr_value()</a>}
</code></pre>




### <a name="type-attr_list">attr_list()</a> ###


<pre><code>
attr_list() = [<a href="#type-attr">attr()</a>]
</code></pre>




### <a name="type-attr_name">attr_name()</a> ###


<pre><code>
attr_name() = binary()
</code></pre>




### <a name="type-attr_value">attr_value()</a> ###


<pre><code>
attr_value() = binary()
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-attr_list">attr_list()</a>)
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(AttrList::<a href="#type-attr_list">attr_list()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

