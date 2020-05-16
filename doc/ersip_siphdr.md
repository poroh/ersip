

# Module ersip_siphdr #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-known_header">known_header()</a> ###


<pre><code>
known_header() = <a href="ersip_hnames.md#type-known_header">ersip_hnames:known_header()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_known_headers-0">all_known_headers/0</a></td><td></td></tr><tr><td valign="top"><a href="#copy_header-3">copy_header/3</a></td><td></td></tr><tr><td valign="top"><a href="#copy_headers-3">copy_headers/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_header-2">parse_header/2</a></td><td></td></tr><tr><td valign="top"><a href="#remove_header-2">remove_header/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_header-3">set_header/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_raw_header-2">set_raw_header/2</a></td><td>Set header to specified value.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_known_headers-0"></a>

### all_known_headers/0 ###

`all_known_headers() -> any()`

<a name="copy_header-3"></a>

### copy_header/3 ###

<pre><code>
copy_header(Header, SrcSipMsg, DstSipMsg) -&gt; NewDstSipMsg
</code></pre>

<ul class="definitions"><li><code>Header = <a href="ersip_hnames.md#type-name_forms">ersip_hnames:name_forms()</a></code></li><li><code>SrcSipMsg = <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a></code></li><li><code>DstSipMsg = <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a></code></li><li><code>NewDstSipMsg = <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a></code></li></ul>

<a name="copy_headers-3"></a>

### copy_headers/3 ###

<pre><code>
copy_headers(HeaderList, SrcSipMsg, DstSipMsg) -&gt; NewDstSipMsg
</code></pre>

<ul class="definitions"><li><code>HeaderList = [<a href="#type-known_header">known_header()</a> | binary()]</code></li><li><code>SrcSipMsg = <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a></code></li><li><code>DstSipMsg = <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a></code></li><li><code>NewDstSipMsg = <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a></code></li></ul>

<a name="parse_header-2"></a>

### parse_header/2 ###

<pre><code>
parse_header(HdrAtom::<a href="#type-known_header">known_header()</a>, Msg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; ValueOrError
</code></pre>

<ul class="definitions"><li><code>ValueOrError = {ok, term()} | {error, term()}</code></li></ul>

<a name="remove_header-2"></a>

### remove_header/2 ###

<pre><code>
remove_header(Header::<a href="ersip_hnames.md#type-name_forms">ersip_hnames:name_forms()</a>, SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>
</code></pre>
<br />

<a name="set_header-3"></a>

### set_header/3 ###

<pre><code>
set_header(Header::<a href="#type-known_header">known_header()</a>, Value::term(), SipMsg::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>
</code></pre>
<br />

<a name="set_raw_header-2"></a>

### set_raw_header/2 ###

<pre><code>
set_raw_header(RawHdr::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>, SipMsg0::<a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>) -&gt; {ok, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>} | {error, term()}
</code></pre>
<br />

Set header to specified value. If this value is already parsed
then also updates parsed cached value.

