

# Module ersip_sdp_bandwidth #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-bandwidth">bandwidth()</a> ###


<pre><code>
bandwidth() = {bandwidth, <a href="#type-bw_list">bw_list()</a>}
</code></pre>




### <a name="type-bw_item">bw_item()</a> ###


<pre><code>
bw_item() = {<a href="#type-bw_type">bw_type()</a>, <a href="#type-bw_value">bw_value()</a>}
</code></pre>




### <a name="type-bw_list">bw_list()</a> ###


<pre><code>
bw_list() = [<a href="#type-bw_item">bw_item()</a>]
</code></pre>




### <a name="type-bw_type">bw_type()</a> ###


<pre><code>
bw_type() = ct | as | tias | {bw_type, binary()}
</code></pre>




### <a name="type-bw_value">bw_value()</a> ###


<pre><code>
bw_value() = non_neg_integer()
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-bandwidth">bandwidth()</a>)
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#as-1">as/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#ct-1">ct/1</a></td><td></td></tr><tr><td valign="top"><a href="#experimental-2">experimental/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#tias-1">tias/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="as-1"></a>

### as/1 ###

<pre><code>
as(X1::<a href="#type-bandwidth">bandwidth()</a>) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(X1::<a href="#type-bandwidth">bandwidth()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="ct-1"></a>

### ct/1 ###

<pre><code>
ct(X1::<a href="#type-bandwidth">bandwidth()</a>) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

<a name="experimental-2"></a>

### experimental/2 ###

<pre><code>
experimental(Name::binary(), X2::<a href="#type-bandwidth">bandwidth()</a>) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

<a name="tias-1"></a>

### tias/1 ###

<pre><code>
tias(X1::<a href="#type-bandwidth">bandwidth()</a>) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

