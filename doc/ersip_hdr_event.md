

# Module ersip_hdr_event #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-event">event()</a> ###


<pre><code>
event() = #event{type = {unknown_event, binary()}, typebin = binary(), hparams = <a href="ersip_hparams.md#type-hparams">ersip_hparams:hparams()</a>}
</code></pre>




### <a name="type-id">id()</a> ###


<pre><code>
id() = binary()
</code></pre>

 lower case.



### <a name="type-type">type()</a> ###


<pre><code>
type() = {unknown_event, binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#id-2">id/2</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#param-2">param/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_param-3">set_param/3</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr><tr><td valign="top"><a href="#type_bin-1">type_bin/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Event::<a href="#type-event">event()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Event::<a href="#type-event">event()</a>) -&gt; binary()
</code></pre>
<br />

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName::binary(), Event::<a href="#type-event">event()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

<a name="id-2"></a>

### id/2 ###

<pre><code>
id(Event::<a href="#type-event">event()</a>, Default::<a href="#type-id">id()</a> | undefined) -&gt; <a href="#type-id">id()</a> | undefined
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-event">event()</a>
</code></pre>
<br />

<a name="param-2"></a>

### param/2 ###

<pre><code>
param(Name::binary(), Event::<a href="#type-event">event()</a>) -&gt; {ok, Value::binary()} | not_found
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary() | <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; {ok, <a href="#type-event">event()</a>} | {error, term()}
</code></pre>
<br />

<a name="set_param-3"></a>

### set_param/3 ###

<pre><code>
set_param(Name::binary(), PValue::binary(), Event::<a href="#type-event">event()</a>) -&gt; <a href="#type-event">event()</a>
</code></pre>
<br />

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(Event::<a href="#type-event">event()</a>) -&gt; <a href="#type-type">type()</a>
</code></pre>
<br />

<a name="type_bin-1"></a>

### type_bin/1 ###

<pre><code>
type_bin(Event::<a href="#type-event">event()</a>) -&gt; binary()
</code></pre>
<br />

