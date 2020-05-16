

# Module ersip_hdr_auth #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-headers">headers()</a> ###


<pre><code>
headers() = [<a href="ersip_authinfo.md#type-authinfo">ersip_authinfo:authinfo()</a>]
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-headers">headers()</a>} | {error, term()}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = [<a href="ersip_authinfo.md#type-raw">ersip_authinfo:raw()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName::binary(), AuthInfos::<a href="#type-headers">headers()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(AuthInfoRawList::<a href="#type-raw">raw()</a>) -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Headers::<a href="#type-headers">headers()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

