

# Module ersip_hdr_expires #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-expires">expires()</a> ###


<pre><code>
expires() = {expires, non_neg_integer()}
</code></pre>




### <a name="type-parse_errors">parse_errors()</a> ###


<pre><code>
parse_errors() = empty_field | binary()
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result(ErrorType) = {ok, <a href="#type-expires">expires()</a>} | {error, {invalid_expires, ErrorType}}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName, X2::<a href="#type-expires">expires()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>

<ul class="definitions"><li><code>HdrName = binary()</code></li></ul>

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary() | non_neg_integer()) -&gt; <a href="#type-expires">expires()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-parse_result">parse_result</a>(<a href="#type-parse_errors">parse_errors()</a>)
</code></pre>
<br />

