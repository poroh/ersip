

# Module ersip_display_name #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-display_name">display_name()</a> ###


<pre><code>
display_name() = {display_name, binary() | [binary()]}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Assemble display name to iolist.</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Assemble display name to binary.</td></tr><tr><td valign="top"><a href="#empty-0">empty/0</a></td><td>Create empty display name.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Create display name from binary.</td></tr><tr><td valign="top"><a href="#parse_dn-1">parse_dn/1</a></td><td>Parse dispalay name and return rest of the string.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Raw representation of display name.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(X1::<a href="#type-display_name">display_name()</a>) -&gt; iolist()
</code></pre>
<br />

Assemble display name to iolist.

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(DN::<a href="#type-display_name">display_name()</a>) -&gt; binary()
</code></pre>
<br />

Assemble display name to binary.

<a name="empty-0"></a>

### empty/0 ###

<pre><code>
empty() -&gt; <a href="#type-display_name">display_name()</a>
</code></pre>
<br />

Create empty display name.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Binary::binary() | <a href="#type-raw">raw()</a>) -&gt; <a href="#type-display_name">display_name()</a>
</code></pre>
<br />

Create display name from binary.

<a name="parse_dn-1"></a>

### parse_dn/1 ###

<pre><code>
parse_dn(Quoted::binary()) -&gt; <a href="ersip_parser_aux.md#type-parse_result">ersip_parser_aux:parse_result</a>(<a href="#type-display_name">display_name()</a>)
</code></pre>
<br />

Parse dispalay name and return rest of the string.

ABNF grammar part:

```
  display-name   =  *(token LWS)/ quoted-string
```

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(X1::<a href="#type-display_name">display_name()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Raw representation of display name. Note that it is unquoted.

Examples:

```
    <<"Alice">> = ersip_display_name:raw(ersip_display_name:make(<<"\"Alice\"">>)).
    <<"Lewis Carroll">> = ersip_display_name:raw(ersip_display_name:make(<<"Lewis Carroll">>)).
    <<"Lewis Carroll">> = ersip_display_name:raw(ersip_display_name:make(<<"\"Lewis Carroll\"">>)).
    <<"Theodore \"Teddy\" Roosevelt">> = ersip_display_name:raw(ersip_display_name:make(<<"Theodore \"Teddy\" Roosevelt">>)).
```

