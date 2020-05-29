

# Module ersip_hparams #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-hparams">hparams()</a> ###


<pre><code>
hparams() = #hparams{order = [<a href="#type-lower_key">lower_key()</a>], orig = #{<a href="#type-lower_key">lower_key()</a> =&gt; {<a href="#type-orig_key">orig_key()</a>, <a href="#type-orig_value">orig_value()</a>}}, parsed = #{<a href="#type-parsed_name">parsed_name()</a> =&gt; {<a href="#type-lower_key">lower_key()</a>, <a href="#type-parsed_value">parsed_value()</a>}, <a href="#type-lower_key">lower_key()</a> =&gt; <a href="#type-parsed_name">parsed_name()</a>}}
</code></pre>




### <a name="type-lower_key">lower_key()</a> ###


<pre><code>
lower_key() = binary()
</code></pre>




### <a name="type-orig_key">orig_key()</a> ###


<pre><code>
orig_key() = binary()
</code></pre>




### <a name="type-orig_value">orig_value()</a> ###


<pre><code>
orig_value() = binary()
</code></pre>




### <a name="type-parse_known_fun">parse_known_fun()</a> ###


<pre><code>
parse_known_fun() = fun((<a href="#type-lower_key">lower_key()</a>, <a href="#type-orig_value">orig_value()</a>) -&gt; <a href="#type-parse_known_fun_result">parse_known_fun_result()</a>)
</code></pre>




### <a name="type-parse_known_fun_result">parse_known_fun_result()</a> ###


<pre><code>
parse_known_fun_result() = {ok, {<a href="#type-parsed_name">parsed_name()</a>, <a href="#type-parsed_value">parsed_value()</a>}} | {ok, unknown} | {error, term()}
</code></pre>




### <a name="type-parsed_name">parsed_name()</a> ###


<pre><code>
parsed_name() = atom()
</code></pre>




### <a name="type-parsed_value">parsed_value()</a> ###


<pre><code>
parsed_value() = term()
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = #{<a href="#type-lower_key">lower_key()</a> =&gt; <a href="#type-orig_value">orig_value()</a>}
</code></pre>




### <a name="type-raw_list">raw_list()</a> ###


<pre><code>
raw_list() = [{<a href="#type-orig_key">orig_key()</a>, <a href="#type-orig_value">orig_value()</a>} | <a href="#type-orig_key">orig_key()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Serialize parameters to iolist.</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Serialize parameters to binary.</td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td>Find parsed or generic parameter.</td></tr><tr><td valign="top"><a href="#find_raw-2">find_raw/2</a></td><td>Find original value of the parameter.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Get value of the parameter.</td></tr><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td>Check if headers params are empty.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Create paramters from binary or from raw data.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Create empty parameters.</td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>Parse parameters and parse all known params.</td></tr><tr><td valign="top"><a href="#parse_known-2">parse_known/2</a></td><td>Enrich parameters with parsed values.</td></tr><tr><td valign="top"><a href="#parse_raw-1">parse_raw/1</a></td><td>Parse paramters from binary.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Represent in play Erlang terms.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>Remove parameter from parameters.</td></tr><tr><td valign="top"><a href="#set-4">set/4</a></td><td>Set parameter and parse it using ParseKnownFun.</td></tr><tr><td valign="top"><a href="#set-5">set/5</a></td><td>Set value of the parameter.</td></tr><tr><td valign="top"><a href="#set_raw-3">set_raw/3</a></td><td>Set original value of the parameter.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>Return list of mixed parsed (known) and unparsed (unknown) parameters.</td></tr><tr><td valign="top"><a href="#to_raw_list-1">to_raw_list/1</a></td><td>Return list of parameters.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Hparams::<a href="#type-hparams">hparams()</a>) -&gt; iolist()
</code></pre>
<br />

Serialize parameters to iolist.

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Hparams::<a href="#type-hparams">hparams()</a>) -&gt; binary()
</code></pre>
<br />

Serialize parameters to binary.

<a name="find-2"></a>

### find/2 ###

<pre><code>
find(ParsedName::<a href="#type-parsed_name">parsed_name()</a> | binary(), Hparams::<a href="#type-hparams">hparams()</a>) -&gt; {ok, <a href="#type-parsed_value">parsed_value()</a>} | not_found
</code></pre>
<br />

Find parsed or generic parameter.

<a name="find_raw-2"></a>

### find_raw/2 ###

<pre><code>
find_raw(BinName::<a href="#type-orig_key">orig_key()</a>, Hparams::<a href="#type-hparams">hparams()</a>) -&gt; {ok, <a href="#type-orig_value">orig_value()</a>} | not_found
</code></pre>
<br />

Find original value of the parameter.

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Name::<a href="#type-parsed_name">parsed_name()</a> | binary(), Hparams::<a href="#type-hparams">hparams()</a>) -&gt; <a href="#type-parsed_value">parsed_value()</a>
</code></pre>
<br />

Get value of the parameter.
Raises error if parameter is not defined.

<a name="is_empty-1"></a>

### is_empty/1 ###

<pre><code>
is_empty(Hparams::<a href="#type-hparams">hparams()</a>) -&gt; boolean()
</code></pre>
<br />

Check if headers params are empty.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary() | <a href="#type-raw">raw()</a>) -&gt; <a href="#type-hparams">hparams()</a>
</code></pre>
<br />

Create paramters from binary or from raw data.
Raises error if parameters cannot be parsed.

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-hparams">hparams()</a>
</code></pre>
<br />

Create empty parameters.

<a name="parse-2"></a>

### parse/2 ###

<pre><code>
parse(ParseKnownF::<a href="#type-parse_known_fun">parse_known_fun()</a>, Bin::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-hparams">hparams()</a>)
</code></pre>
<br />

Parse parameters and parse all known params.

<a name="parse_known-2"></a>

### parse_known/2 ###

<pre><code>
parse_known(ParseKnownFun::<a href="#type-parse_known_fun">parse_known_fun()</a>, Hparams::<a href="#type-hparams">hparams()</a>) -&gt; {ok, <a href="#type-hparams">hparams()</a>} | {error, term()}
</code></pre>
<br />

Enrich parameters with parsed values.  ParseKnownFun should
return result of the parameter parsing (see type definition).

<a name="parse_raw-1"></a>

### parse_raw/1 ###

<pre><code>
parse_raw(Binary::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-hparams">hparams()</a>)
</code></pre>
<br />

Parse paramters from binary.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Hparams::<a href="#type-hparams">hparams()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Represent in play Erlang terms.

<a name="remove-2"></a>

### remove/2 ###

<pre><code>
remove(BinName::binary() | <a href="#type-parsed_name">parsed_name()</a>, Hparams::<a href="#type-hparams">hparams()</a>) -&gt; <a href="#type-hparams">hparams()</a>
</code></pre>
<br />

Remove parameter from parameters.

<a name="set-4"></a>

### set/4 ###

<pre><code>
set(PName::binary(), PValue::binary(), ParseKnownF::<a href="#type-parse_known_fun">parse_known_fun()</a>, HParams::<a href="ersip_hparams.md#type-hparams">ersip_hparams:hparams()</a>) -&gt; {ok, <a href="ersip_hparams.md#type-hparams">ersip_hparams:hparams()</a>} | {error, term()}
</code></pre>
<br />

Set parameter and parse it using ParseKnownFun.

<a name="set-5"></a>

### set/5 ###

<pre><code>
set(ParsedName::<a href="#type-parsed_name">parsed_name()</a>, ParsedValue::<a href="#type-parsed_value">parsed_value()</a>, Key::<a href="#type-orig_key">orig_key()</a>, Value::<a href="#type-orig_value">orig_value()</a>, Hparams::<a href="#type-hparams">hparams()</a>) -&gt; <a href="#type-hparams">hparams()</a>
</code></pre>
<br />

Set value of the parameter.

<a name="set_raw-3"></a>

### set_raw/3 ###

<pre><code>
set_raw(Key::<a href="#type-orig_key">orig_key()</a>, Value::<a href="#type-orig_value">orig_value()</a>, Hparams::<a href="#type-hparams">hparams()</a>) -&gt; <a href="#type-hparams">hparams()</a>
</code></pre>
<br />

Set original value of the parameter.

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(Hparams::<a href="#type-hparams">hparams()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = [Item]</code></li><li><code>Item = {<a href="#type-parsed_name">parsed_name()</a>, <a href="#type-parsed_value">parsed_value()</a>} | {<a href="#type-lower_key">lower_key()</a>, <a href="#type-orig_value">orig_value()</a>}</code></li></ul>

Return list of mixed parsed (known) and unparsed (unknown) parameters.

<a name="to_raw_list-1"></a>

### to_raw_list/1 ###

<pre><code>
to_raw_list(Hparams::<a href="#type-hparams">hparams()</a>) -&gt; <a href="#type-raw_list">raw_list()</a>
</code></pre>
<br />

Return list of parameters. Note that keys will be in original case.

