

# Module ersip_parser_aux #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-gen_param">gen_param()</a> ###


<pre><code>
gen_param() = {Key::binary(), <a href="#type-gen_param_value">gen_param_value()</a>}
</code></pre>




### <a name="type-gen_param_list">gen_param_list()</a> ###


<pre><code>
gen_param_list() = [<a href="#type-gen_param">gen_param()</a>]
</code></pre>




### <a name="type-gen_param_value">gen_param_value()</a> ###


<pre><code>
gen_param_value() = binary()
</code></pre>




### <a name="type-parse_kvps_validator">parse_kvps_validator()</a> ###


<pre><code>
parse_kvps_validator() = fun((Key::binary(), MayBeValue::novalue | binary()) -&gt; {ok, {Key::term(), Value::term()}} | {error, term()} | skip)
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = <a href="#type-parse_result">parse_result</a>(term())
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result(T, Err) = {ok, ParseResult::T, Rest::binary()} | {error, Err}
</code></pre>




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result(T) = <a href="#type-parse_result">parse_result</a>(T, term())
</code></pre>




### <a name="type-parser_fun">parser_fun()</a> ###


<pre><code>
parser_fun() = fun((binary()) -&gt; <a href="#type-parse_result">parse_result()</a>)
</code></pre>




### <a name="type-separator">separator()</a> ###


<pre><code>
separator() = lws
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#check_token-1">check_token/1</a></td><td>Check binary is token
token       =  1*(alphanum / "-" / "." / "!" / "%" / "*"
/ "_" / "+" / "<code>" / "</code>" / "~" ).</td></tr><tr><td valign="top"><a href="#parse_all-2">parse_all/2</a></td><td>Apply series of parsers:.</td></tr><tr><td valign="top"><a href="#parse_gen_param_value-1">parse_gen_param_value/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_kvps-3">parse_kvps/3</a></td><td>Parse key-value pairs sepeated with Sep.</td></tr><tr><td valign="top"><a href="#parse_lws-1">parse_lws/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_non_neg_int-1">parse_non_neg_int/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_params-2">parse_params/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_sep-2">parse_sep/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_slash-1">parse_slash/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_token-1">parse_token/1</a></td><td>Parse SIP token.</td></tr><tr><td valign="top"><a href="#quoted_string-1">quoted_string/1</a></td><td>Parse quoted string (with unquoute).</td></tr><tr><td valign="top"><a href="#token_list-2">token_list/2</a></td><td>Parse token list separated with SEP.</td></tr><tr><td valign="top"><a href="#trim_lws-1">trim_lws/1</a></td><td></td></tr><tr><td valign="top"><a href="#unquoted_string-1">unquoted_string/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="check_token-1"></a>

### check_token/1 ###

<pre><code>
check_token(Bin::binary()) -&gt; boolean()
</code></pre>
<br />

Check binary is token
token       =  1*(alphanum / "-" / "." / "!" / "%" / "*"
/ "_" / "+" / "`" / "`" / "~" )

<a name="parse_all-2"></a>

### parse_all/2 ###

<pre><code>
parse_all(Binary::binary(), Parsers::[ParserFun]) -&gt; ParseAllResult
</code></pre>

<ul class="definitions"><li><code>ParserFun = fun((binary()) -&gt; <a href="#type-parse_result">parse_result()</a>)</code></li><li><code>ParseAllResult = {ok, [ParseResult], Rest::binary()} | {error, term()}</code></li><li><code>ParseResult = term()</code></li></ul>

Apply series of parsers:

<a name="parse_gen_param_value-1"></a>

### parse_gen_param_value/1 ###

<pre><code>
parse_gen_param_value(Bin::binary()) -&gt; <a href="#type-parse_result">parse_result</a>(<a href="#type-gen_param_value">gen_param_value()</a>)
</code></pre>
<br />

<a name="parse_kvps-3"></a>

### parse_kvps/3 ###

<pre><code>
parse_kvps(Validator, Sep, Bin::binary()) -&gt; <a href="#type-parse_result">parse_result</a>([{Key, Value} | Key])
</code></pre>

<ul class="definitions"><li><code>Key = any()</code></li><li><code>Value = any()</code></li><li><code>Sep = binary()</code></li><li><code>Validator = <a href="#type-parse_kvps_validator">parse_kvps_validator()</a></code></li></ul>

Parse key-value pairs sepeated with Sep.
Validator may:
- transform key-value to another key/value pair
- skip key-value pair
- return error on pair

<a name="parse_lws-1"></a>

### parse_lws/1 ###

<pre><code>
parse_lws(Bin::binary()) -&gt; <a href="#type-parse_result">parse_result</a>({lws, pos_integer()})
</code></pre>
<br />

<a name="parse_non_neg_int-1"></a>

### parse_non_neg_int/1 ###

<pre><code>
parse_non_neg_int(Bin::binary()) -&gt; <a href="#type-parse_result">parse_result</a>(non_neg_integer(), {invalid_integer, binary()})
</code></pre>
<br />

<a name="parse_params-2"></a>

### parse_params/2 ###

<pre><code>
parse_params(Sep::char(), Bin::binary()) -&gt; <a href="#type-parse_result">parse_result</a>(<a href="#type-gen_param_list">gen_param_list()</a>)
</code></pre>
<br />

<a name="parse_sep-2"></a>

### parse_sep/2 ###

<pre><code>
parse_sep(Sep::char(), Bin::binary()) -&gt; <a href="#type-parse_result">parse_result</a>(char())
</code></pre>
<br />

<a name="parse_slash-1"></a>

### parse_slash/1 ###

<pre><code>
parse_slash(Binary::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result()</a>
</code></pre>
<br />

<a name="parse_token-1"></a>

### parse_token/1 ###

<pre><code>
parse_token(Bin::binary()) -&gt; <a href="#type-parse_result">parse_result</a>(binary(), {not_a_token, binary()})
</code></pre>
<br />

Parse SIP token

<a name="quoted_string-1"></a>

### quoted_string/1 ###

<pre><code>
quoted_string(Quoted) -&gt; {ok, Quoted, Rest} | error
</code></pre>

<ul class="definitions"><li><code>Quoted = binary()</code></li><li><code>Quoted = binary()</code></li><li><code>Rest = binary()</code></li></ul>

Parse quoted string (with unquoute).
DQUOTE *(qdtext / quoted-pair ) DQUOTE
qdtext         =  LWS / %x21 / %x23-5B / %x5D-7E
/ UTF8-NONASCII

<a name="token_list-2"></a>

### token_list/2 ###

<pre><code>
token_list(Binary::binary(), SEP) -&gt; {ok, [Token, ...], Rest} | error
</code></pre>

<ul class="definitions"><li><code>SEP = <a href="#type-separator">separator()</a></code></li><li><code>Token = binary()</code></li><li><code>Rest = binary()</code></li></ul>

Parse token list separated with SEP

<a name="trim_lws-1"></a>

### trim_lws/1 ###

<pre><code>
trim_lws(Bin::binary()) -&gt; {ok, {lws, pos_integer()}, Rest::binary()}
</code></pre>
<br />

<a name="unquoted_string-1"></a>

### unquoted_string/1 ###

<pre><code>
unquoted_string(Quoted::binary()) -&gt; <a href="#type-parse_result">parse_result</a>(binary())
</code></pre>
<br />

