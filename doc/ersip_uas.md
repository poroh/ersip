

# Module ersip_uas #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-options">options()</a> ###


<pre><code>
options() = #{supported =&gt; <a href="ersip_hdr_opttag_list.md#type-option_tag_list">ersip_hdr_opttag_list:option_tag_list()</a>, to_tag =&gt; auto | <a href="ersip_hdr_fromto.md#type-tag">ersip_hdr_fromto:tag()</a>, check_scheme =&gt; fun((binary()) -&gt; boolean())}
</code></pre>




### <a name="type-process_result">process_result()</a> ###


<pre><code>
process_result() = {reply, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>} | {process, <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>}
</code></pre>




### <a name="type-result">result()</a> ###


<pre><code>
result() = <a href="#type-process_result">process_result()</a> | {error, {parse_error, term()}}
</code></pre>




### <a name="type-uas">uas()</a> ###


<pre><code>
uas() = #uas{allowed_methods = <a href="ersip_method_set.md#type-set">ersip_method_set:set()</a>, request = undefined | <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, options = <a href="#type-options">options()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#process_request-3">process_request/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="process_request-3"></a>

### process_request/3 ###

<pre><code>
process_request(Message::<a href="ersip_msg.md#type-message">ersip_msg:message()</a> | <a href="ersip_sipmsg.md#type-sipmsg">ersip_sipmsg:sipmsg()</a>, AllowedMethods::<a href="ersip_method_set.md#type-set">ersip_method_set:set()</a>, Options::<a href="#type-options">options()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

