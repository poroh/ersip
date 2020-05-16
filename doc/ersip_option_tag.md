

# Module ersip_option_tag #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-option_tag">option_tag()</a> ###


<pre><code>
option_tag() = {option_tag, binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-option_tag">option_tag()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary()) -&gt; {ok, <a href="#type-option_tag">option_tag()</a>} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Error = {invalid_option_tag, binary()}</code></li></ul>

<a name="to_binary-1"></a>

### to_binary/1 ###

<pre><code>
to_binary(X1::<a href="#type-option_tag">option_tag()</a>) -&gt; binary()
</code></pre>
<br />

