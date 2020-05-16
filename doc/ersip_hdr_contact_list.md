

# Module ersip_hdr_contact_list #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-contact_list">contact_list()</a> ###


<pre><code>
contact_list() = <a href="#type-star_contact">star_contact()</a> | [<a href="ersip_hdr_contact.md#type-contact">ersip_hdr_contact:contact()</a>]
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-contact_list">contact_list()</a>} | {error, term()}
</code></pre>




### <a name="type-star_contact">star_contact()</a> ###


<pre><code>
star_contact() = star
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_star-0">make_star/0</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HeaderName::binary(), ContactList::<a href="#type-contact_list">contact_list()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Binary::iolist() | binary()) -&gt; <a href="#type-contact_list">contact_list()</a>
</code></pre>
<br />

<a name="make_star-0"></a>

### make_star/0 ###

<pre><code>
make_star() -&gt; <a href="#type-star_contact">star_contact()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Header::<a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

