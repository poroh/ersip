

# Module ersip_quoted_string #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-skip_result">skip_result()</a> ###


<pre><code>
skip_result() = {ok, Rest::binary()} | error
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#quote-1">quote/1</a></td><td></td></tr><tr><td valign="top"><a href="#skip-1">skip/1</a></td><td></td></tr><tr><td valign="top"><a href="#unquote-1">unquote/1</a></td><td></td></tr><tr><td valign="top"><a href="#unquoting_parse-1">unquoting_parse/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="quote-1"></a>

### quote/1 ###

<pre><code>
quote(String::binary()) -&gt; binary()
</code></pre>
<br />

<a name="skip-1"></a>

### skip/1 ###

<pre><code>
skip(String::binary()) -&gt; <a href="#type-skip_result">skip_result()</a>
</code></pre>
<br />

<a name="unquote-1"></a>

### unquote/1 ###

<pre><code>
unquote(V::binary()) -&gt; binary()
</code></pre>
<br />

<a name="unquoting_parse-1"></a>

### unquoting_parse/1 ###

<pre><code>
unquoting_parse(Quoted::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(binary())
</code></pre>
<br />

