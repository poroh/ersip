

# Module ersip_status #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-code">code()</a> ###


<pre><code>
code() = 100..699
</code></pre>




### <a name="type-reason">reason()</a> ###


<pre><code>
reason() = binary()
</code></pre>




### <a name="type-response_type">response_type()</a> ###


<pre><code>
response_type() = final | provisional
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bad_request_reason-1">bad_request_reason/1</a></td><td></td></tr><tr><td valign="top"><a href="#reason_phrase-1">reason_phrase/1</a></td><td></td></tr><tr><td valign="top"><a href="#response_type-1">response_type/1</a></td><td>Convert status code to response type (final or provisional).</td></tr><tr><td valign="top"><a href="#unsupported_uri_scheme_reason-1">unsupported_uri_scheme_reason/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bad_request_reason-1"></a>

### bad_request_reason/1 ###

`bad_request_reason(X1) -> any()`

<a name="reason_phrase-1"></a>

### reason_phrase/1 ###

<pre><code>
reason_phrase(Code::<a href="#type-code">code()</a>) -&gt; <a href="#type-reason">reason()</a>
</code></pre>
<br />

<a name="response_type-1"></a>

### response_type/1 ###

<pre><code>
response_type(StatusCode::<a href="#type-code">code()</a>) -&gt; <a href="#type-response_type">response_type()</a>
</code></pre>
<br />

Convert status code to response type (final or provisional).

<a name="unsupported_uri_scheme_reason-1"></a>

### unsupported_uri_scheme_reason/1 ###

<pre><code>
unsupported_uri_scheme_reason(URIScheme::<a href="ersip_uri.md#type-scheme">ersip_uri:scheme()</a>) -&gt; <a href="#type-reason">reason()</a>
</code></pre>
<br />

