

# Module ersip_authinfo #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-authinfo">authinfo()</a> ###


<pre><code>
authinfo() = #authinfo{type = binary(), params = <a href="ersip_parser_aux.md#type-gen_param_list">ersip_parser_aux:gen_param_list()</a>}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = {Type::binary(), [{binary(), binary()}]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td></td></tr><tr><td valign="top"><a href="#raw_param-2">raw_param/2</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Authinfo::<a href="#type-authinfo">authinfo()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Authinfo::<a href="#type-authinfo">authinfo()</a>) -&gt; binary()
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary() | <a href="#type-raw">raw()</a>) -&gt; <a href="#type-authinfo">authinfo()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary()) -&gt; {ok, <a href="#type-authinfo">authinfo()</a>} | {error, term()}
</code></pre>
<br />

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Authinfo::<a href="#type-authinfo">authinfo()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

<a name="raw_param-2"></a>

### raw_param/2 ###

<pre><code>
raw_param(Name::binary(), Authinfo::<a href="#type-authinfo">authinfo()</a>) -&gt; {ok, binary()} | undefined
</code></pre>
<br />

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(Authinfo::<a href="#type-authinfo">authinfo()</a>) -&gt; binary()
</code></pre>
<br />

