

# Module ersip_qvalue #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-parse_result">parse_result()</a> ###


<pre><code>
parse_result() = {ok, <a href="#type-qvalue">qvalue()</a>} | {error, {invalid_qvalue, binary()}}
</code></pre>




### <a name="type-qvalue">qvalue()</a> ###


<pre><code>
qvalue() = {qvalue, 0..1000}
</code></pre>




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = 0..1000
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(X1::<a href="#type-qvalue">qvalue()</a>) -&gt; binary()
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-qvalue">qvalue()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary()) -&gt; <a href="#type-parse_result">parse_result()</a>
</code></pre>
<br />

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(X1::<a href="#type-qvalue">qvalue()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

