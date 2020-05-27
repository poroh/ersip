

# Module ersip_hdr_maxforwards #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-maxforwards">maxforwards()</a> ###


<pre><code>
maxforwards() = {maxforwards, non_neg_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#dec-1">dec/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName, X2::<a href="#type-maxforwards">maxforwards()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>

<ul class="definitions"><li><code>HdrName = binary()</code></li></ul>

<a name="dec-1"></a>

### dec/1 ###

<pre><code>
dec(X1::<a href="#type-maxforwards">maxforwards()</a>) -&gt; <a href="#type-maxforwards">maxforwards()</a>
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Number::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a> | binary() | non_neg_integer()) -&gt; <a href="#type-maxforwards">maxforwards()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, <a href="#type-maxforwards">maxforwards()</a>} | {error, Error}</code></li><li><code>Error = no_maxforwards | {invalid_maxforwards, binary()}</code></li></ul>

<a name="value-1"></a>

### value/1 ###

<pre><code>
value(X1::<a href="#type-maxforwards">maxforwards()</a>) -&gt; non_neg_integer()
</code></pre>
<br />
