

# Module ersip_hnames #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-header_key">header_key()</a> ###


<pre><code>
header_key() = {hdr_key, atom() | binary()}
</code></pre>




### <a name="type-known_header">known_header()</a> ###


<pre><code>
known_header() = from | to | callid | cseq | maxforwards | topmost_via | content_type | allow | route | record_route | supported | unsupported | require | proxy_require | contact | expires | minexpires | date | www_authenticate | authorization | proxy_authenticate | proxy_authorization | subscription_state | event | refer_to | replaces | rseq
</code></pre>




### <a name="type-name_forms">name_forms()</a> ###


<pre><code>
name_forms() = <a href="#type-header_key">header_key()</a> | <a href="#type-known_header">known_header()</a> | binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_known_headers-0">all_known_headers/0</a></td><td></td></tr><tr><td valign="top"><a href="#known_header_form-1">known_header_form/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_key-1">make_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_known_key-1">make_known_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#print_form-1">print_form/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_known_headers-0"></a>

### all_known_headers/0 ###

`all_known_headers() -> any()`

<a name="known_header_form-1"></a>

### known_header_form/1 ###

<pre><code>
known_header_form(HeaderName::binary() | <a href="#type-header_key">header_key()</a>) -&gt; {ok, <a href="#type-known_header">known_header()</a>} | not_found
</code></pre>
<br />

<a name="make_key-1"></a>

### make_key/1 ###

<pre><code>
make_key(Key::<a href="#type-name_forms">name_forms()</a>) -&gt; <a href="#type-header_key">header_key()</a>
</code></pre>
<br />

<a name="make_known_key-1"></a>

### make_known_key/1 ###

<pre><code>
make_known_key(KnownHeader::atom()) -&gt; <a href="#type-header_key">header_key()</a> | not_found
</code></pre>
<br />

<a name="print_form-1"></a>

### print_form/1 ###

<pre><code>
print_form(HeaderForm::<a href="#type-name_forms">name_forms()</a>) -&gt; binary()
</code></pre>
<br />

