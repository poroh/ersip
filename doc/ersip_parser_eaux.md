

# Module ersip_parser_eaux #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-context">context()</a> ###


<pre><code>
context() = #context{str = string(), bin = binary(), acc = string(), opts = <a href="#type-options">options()</a>}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = #{}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = <a href="#type-parse_result">parse_result</a>(term())
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result(T, Err) = {ok, ParseResult::T, Context::<a href="#type-context">context()</a>} | {error, Err}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result(T) = <a href="#type-parse_result">parse_result</a>(T, term())
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#colon-1">colon/1</a></td><td></td></tr><tr><td valign="top"><a href="#prepare-1">prepare/1</a></td><td></td></tr><tr><td valign="top"><a href="#rest-1">rest/1</a></td><td></td></tr><tr><td valign="top"><a href="#sequence-2">sequence/2</a></td><td>Apply series of parsers:.</td></tr><tr><td valign="top"><a href="#skip_lws-1">skip_lws/1</a></td><td></td></tr><tr><td valign="top"><a href="#token-1">token/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="colon-1"></a>

### colon/1 ###

<pre><code>
colon(Context::<a href="#type-context">context()</a>) -&gt; <a href="#type-parse_result">parse_result</a>(colon)
</code></pre>
<br />

<a name="prepare-1"></a>

### prepare/1 ###

<pre><code>
prepare(Binary::binary()) -&gt; <a href="#type-context">context()</a>
</code></pre>
<br />

<a name="rest-1"></a>

### rest/1 ###

<pre><code>
rest(Context::<a href="#type-context">context()</a>) -&gt; binary()
</code></pre>
<br />

<a name="sequence-2"></a>

### sequence/2 ###

<pre><code>
sequence(Context::<a href="#type-context">context()</a>, Parsers::[ParserFun]) -&gt; ParseAllResult
</code></pre>

<ul class="definitions"><li><code>ParserFun = fun((<a href="#type-context">context()</a>) -&gt; <a href="#type-parse_result">parse_result()</a>)</code></li><li><code>ParseAllResult = {ok, [ParseResult], Context::<a href="#type-context">context()</a>} | {error, term()}</code></li><li><code>ParseResult = term()</code></li></ul>

Apply series of parsers:

<a name="skip_lws-1"></a>

### skip_lws/1 ###

<pre><code>
skip_lws(Context::<a href="#type-context">context()</a>) -&gt; <a href="#type-parse_result">parse_result</a>(lws)
</code></pre>
<br />

<a name="token-1"></a>

### token/1 ###

<pre><code>
token(Context::<a href="#type-context">context()</a>) -&gt; <a href="#type-parse_result">parse_result</a>(binary(), not_a_token)
</code></pre>
<br />

