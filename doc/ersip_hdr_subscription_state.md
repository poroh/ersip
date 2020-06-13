

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




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = #{value =&gt; binary(), params =&gt; <a href="ersip_hparams.md#type-raw">ersip_hparams:raw()</a>, reason =&gt; binary(), retry_after =&gt; non_neg_integer(), expires =&gt; non_neg_integer()}
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assemble-1">assemble/1</a></td><td>Assemble Subscription-State to iolist().</td></tr><tr><td valign="top"><a href="#assemble_bin-1">assemble_bin/1</a></td><td>Assemble Subscription-State to binary().</td></tr><tr><td valign="top"><a href="#build-2">build/2</a></td><td>Build raw SIP header from Subscription-State header.</td></tr><tr><td valign="top"><a href="#event_reason_value-1">event_reason_value/1</a></td><td>Value of reason parameter (deactivated/probation/rejected/noresource/...).</td></tr><tr><td valign="top"><a href="#event_reason_value-2">event_reason_value/2</a></td><td>Value of reason parameter (deactivated/probation/rejected/noresource/...).</td></tr><tr><td valign="top"><a href="#expires-1">expires/1</a></td><td>Value of the expires prameter.</td></tr><tr><td valign="top"><a href="#expires-2">expires/2</a></td><td>Value of the expires prameter.</td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td>Make Subscription-State header from binary or raw
representation of parameter.</td></tr><tr><td valign="top"><a href="#param-2">param/2</a></td><td>Value of any parameter in raw form.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Parse Subscription-State header from binary or raw SIP header.</td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td>Raw representation of Subscription-State header.</td></tr><tr><td valign="top"><a href="#retry_after-1">retry_after/1</a></td><td>Value of the retry-after parameter.</td></tr><tr><td valign="top"><a href="#retry_after-2">retry_after/2</a></td><td>Value of the retry-after parameter.</td></tr><tr><td valign="top"><a href="#set_event_reason_value-2">set_event_reason_value/2</a></td><td>Set value of reason parameter.</td></tr><tr><td valign="top"><a href="#set_expires-2">set_expires/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_param-3">set_param/3</a></td><td>Set value of any parameter in raw form.</td></tr><tr><td valign="top"><a href="#set_retry_after-2">set_retry_after/2</a></td><td>Set retry after parameter.</td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td>Main value of the subscription state (active/pending/terminated).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assemble-1"></a>

### assemble/1 ###

<pre><code>
assemble(Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; iolist()
</code></pre>
<br />

Assemble Subscription-State to iolist().

<a name="assemble_bin-1"></a>

### assemble_bin/1 ###

<pre><code>
assemble_bin(Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; binary()
</code></pre>
<br />

Assemble Subscription-State to binary().

<a name="build-2"></a>

### build/2 ###

<pre><code>
build(HdrName::binary(), Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>
</code></pre>
<br />

Build raw SIP header from Subscription-State header.

<a name="event_reason_value-1"></a>

### event_reason_value/1 ###

<pre><code>
event_reason_value(Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; <a href="#type-event_reason_value">event_reason_value()</a> | undefined
</code></pre>
<br />

Value of reason parameter (deactivated/probation/rejected/noresource/...)

<a name="event_reason_value-2"></a>

### event_reason_value/2 ###

<pre><code>
event_reason_value(Subs_state::<a href="#type-subs_state">subs_state()</a>, Default::<a href="#type-event_reason_value">event_reason_value()</a> | undefined) -&gt; <a href="#type-event_reason_value">event_reason_value()</a> | undefined
</code></pre>
<br />

Value of reason parameter (deactivated/probation/rejected/noresource/...)

<a name="expires-1"></a>

### expires/1 ###

<pre><code>
expires(Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

Value of the expires prameter.

<a name="expires-2"></a>

### expires/2 ###

<pre><code>
expires(Subs_state::<a href="#type-subs_state">subs_state()</a>, Default::non_neg_integer() | undefined) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

Value of the expires prameter.

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(Bin::binary()) -&gt; <a href="#type-subs_state">subs_state()</a>
</code></pre>
<br />

Make Subscription-State header from binary or raw
representation of parameter.

<a name="param-2"></a>

### param/2 ###

<pre><code>
param(Name::binary(), Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; {ok, Value::binary()} | not_found
</code></pre>
<br />

Value of any parameter in raw form.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Bin::binary() | <a href="ersip_hdr.md#type-header">ersip_hdr:header()</a>) -&gt; {ok, <a href="#type-subs_state">subs_state()</a>} | {error, term()}
</code></pre>
<br />

Parse Subscription-State header from binary or raw SIP header.

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

Raw representation of Subscription-State header.

<a name="retry_after-1"></a>

### retry_after/1 ###

<pre><code>
retry_after(Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

Value of the retry-after parameter.

<a name="retry_after-2"></a>

### retry_after/2 ###

<pre><code>
retry_after(Subs_state::<a href="#type-subs_state">subs_state()</a>, Default::non_neg_integer() | undefined) -&gt; non_neg_integer() | undefined
</code></pre>
<br />

Value of the retry-after parameter.

<a name="set_event_reason_value-2"></a>

### set_event_reason_value/2 ###

<pre><code>
set_event_reason_value(Val::binary() | <a href="#type-event_reason_value">event_reason_value()</a>, Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; <a href="#type-subs_state">subs_state()</a>
</code></pre>
<br />

Set value of reason parameter.

<a name="set_expires-2"></a>

### set_expires/2 ###

<pre><code>
set_expires(Expires::non_neg_integer(), Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; <a href="#type-subs_state">subs_state()</a>
</code></pre>
<br />

<a name="set_param-3"></a>

### set_param/3 ###

<pre><code>
set_param(Name::binary(), PValue::binary(), Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; <a href="#type-subs_state">subs_state()</a>
</code></pre>
<br />

Set value of any parameter in raw form.

<a name="set_retry_after-2"></a>

### set_retry_after/2 ###

<pre><code>
set_retry_after(RetryAfter::non_neg_integer(), Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; <a href="#type-subs_state">subs_state()</a>
</code></pre>
<br />

Set retry after parameter.

<a name="value-1"></a>

### value/1 ###

<pre><code>
value(Subs_state::<a href="#type-subs_state">subs_state()</a>) -&gt; <a href="#type-value">value()</a>
</code></pre>
<br />

Main value of the subscription state (active/pending/terminated).

