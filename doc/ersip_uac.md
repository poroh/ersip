

# Module ersip_uac #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-header">header()</a> ###


<pre><code>
header() = <a href="ersip_hnames.md#type-known_header">ersip_hnames:known_header()</a> | binary()
</code></pre>




### <a name="type-req_param">req_param()</a> ###


<pre><code>
req_param() = {<a href="#type-header">header()</a>, <a href="#type-value">value()</a>}
</code></pre>




### <a name="type-req_params">req_params()</a> ###


<pre><code>
req_params() = [<a href="#type-req_param">req_param()</a>]
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = any()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#options-3">options/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="options-3"></a>

### options/3 ###

<pre><code>
options(RURI::<a href="ersip_uri.md#type-uri">ersip_uri:uri()</a>, From::<a href="ersip_hdr_fromto.md#type-fromto">ersip_hdr_fromto:fromto()</a>, ReqParams::<a href="#type-req_params">req_params()</a>) -&gt; <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>
</code></pre>
<br />

