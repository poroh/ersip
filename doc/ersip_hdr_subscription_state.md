

# Module ersip_hdr_subscription_state #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-event_reason_value">event_reason_value()</a> ###


<pre><code>
event_reason_value() = deactivated | probation | rejected | timeout | giveup | noresource | invariant | {unknown_reason, binary()}
</code></pre>




### <a name="type-subs_state">subs_state()</a> ###


<pre><code>
subs_state() = #subs_state{value = <a href="#type-value">value()</a>, valuebin = binary(), hparams = <a href="ersip_hparams.md#type-hparams">ersip_hparams:hparams()</a>}
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = active | pending | terminated | {unknown, binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td></td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td></td></tr><tr><td valign="top"><a href="#event_reason_value-2">event_reason_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#expires-2">expires/2</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#param-2">param/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#retry_after-2">retry_after/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_param-3">set_param/3</a></td><td></td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; binary()
</code></pre>
<br />

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName::binary(), Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

<a name="event_reason_value-2"></a>

### event_reason_value/2 ###

<pre><code>
event_reason_value(Subs_state::<a href="#type-subs_state">subs_state()</a>, Default::<a href="#type-event_reason_value">event_reason_value()</a> | undefined) -&gt; <a href="#type-event_reason_value">event_reason_value()</a> | undefined
</code></pre>
<br />

<a name="expires-2"></a>

### expires/2 ###

<pre><code>
expires(Subs_state::<a href="#type-subs_state">subs_state()</a>, Default::non_neg_integer() | undefined) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-subs_state">subs_state()</a>
</code></pre>
<br />

<a name="param-2"></a>

### param/2 ###

<pre><code>
param(Name::binary(), Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; {ok, Value::binary()} | not_found
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary() | <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; {ok, <a href="#type-subs_state">subs_state()</a>} | {error, term()}
</code></pre>
<br />

<a name="retry_after-2"></a>

### retry_after/2 ###

<pre><code>
retry_after(Subs_state::<a href="#type-subs_state">subs_state()</a>, Default::non_neg_integer() | undefined) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

<a name="set_param-3"></a>

### set_param/3 ###

<pre><code>
set_param(Name::binary(), PValue::binary(), Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; <a href="#type-subs_state">subs_state()</a>
</code></pre>
<br />

<a name="value-1"></a>

### value/1 ###

<pre><code>
value(Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; <a href="#type-value">value()</a>
</code></pre>
<br />

