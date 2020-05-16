

# Module ersip_parser #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-data">data()</a> ###


<pre><code>
data() = #data{options = map(), buf = <a href="ersip_buf.md#type-state">ersip_buf:state()</a>, state = <a href="#type-state">state()</a>, message = <a href="ersip.md#type-message">ersip:message()</a>, acc = [binary()], content_len = pos_integer() | undefined, start_pos = non_neg_integer()}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = #{buffer =&gt; <a href="ersip_buf.md#type-options">ersip_buf:options()</a>, max_message_len =&gt; unlimited | pos_integer()}
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = first_line | headers | body
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_binary-2">add_binary/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new_dgram-1">new_dgram/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_binary-2"></a>

### add_binary/2 ###

<pre><code>
add_binary(Binary::binary(), Data::<a href="#type-data">data()</a>) -&gt; <a href="#type-data">data()</a>
</code></pre>
<br />

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-data">data()</a>
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Options::<a href="#type-options">options()</a>) -&gt; <a href="#type-data">data()</a>
</code></pre>
<br />

<a name="new_dgram-1"></a>

### new_dgram/1 ###

<pre><code>
new_dgram(DatagramBinary::binary()) -&gt; <a href="#type-data">data()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

`parse(Data) -> any()`

