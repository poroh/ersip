

# Module ersip_reply #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-know_param">know_param()</a> ###


<pre><code>
know_param() = reason | to_tag
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = #options{status = <a href="ersip_status.md#type-code">ersip_status:code()</a>, reason = undefined | <a href="ersip_status.md#type-reason">ersip_status:reason()</a>, to_tag = auto | <a href="ersip_hdr_fromto.md#type-tag">ersip_hdr_fromto:tag()</a>}
</code></pre>




### <a name="type-param_pair">param_pair()</a> ###


<pre><code>
param_pair() = {<a href="#type-know_param">know_param()</a>, term()}
</code></pre>




### <a name="type-params_list">params_list()</a> ###


<pre><code>
params_list() = [<a href="#type-param_pair">param_pair()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#reason-1">reason/1</a></td><td></td></tr><tr><td valign="top"><a href="#status-1">status/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_tag-1">to_tag/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Status::<a href="ersip_status.md#type-code">ersip_status:code()</a>) -&gt; <a href="#type-options">options()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Status::<a href="ersip_status.md#type-code">ersip_status:code()</a>, Params::<a href="#type-params_list">params_list()</a>) -&gt; <a href="#type-options">options()</a>
</code></pre>
<br />

<a name="reason-1"></a>

### reason/1 ###

<pre><code>
reason(Options::#options{status = <a href="ersip_status.md#type-code">ersip_status:code()</a>, reason = undefined | <a href="ersip_status.md#type-reason">ersip_status:reason()</a>, to_tag = auto | <a href="ersip_hdr_fromto.md#type-tag">ersip_hdr_fromto:tag()</a>}) -&gt; binary()
</code></pre>
<br />

<a name="status-1"></a>

### status/1 ###

<pre><code>
status(Options::#options{status = <a href="ersip_status.md#type-code">ersip_status:code()</a>, reason = undefined | <a href="ersip_status.md#type-reason">ersip_status:reason()</a>, to_tag = auto | <a href="ersip_hdr_fromto.md#type-tag">ersip_hdr_fromto:tag()</a>}) -&gt; <a href="ersip_status.md#type-code">ersip_status:code()</a>
</code></pre>
<br />

<a name="to_tag-1"></a>

### to_tag/1 ###

<pre><code>
to_tag(Options::<a href="#type-options">options()</a>) -&gt; auto | <a href="ersip_hdr_fromto.md#type-tag">ersip_hdr_fromto:tag()</a>
</code></pre>
<br />

