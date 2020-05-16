

# Module ersip_ua_inv_session #
* [Data Types](#types)

<a name="types"></a>

## Data Types ##




### <a name="type-inv_session">inv_session()</a> ###


<pre><code>
inv_session() = #inv_session{state = <a href="#type-state">state()</a>, dialog = <a href="ersip_dialog.md#type-dialog">ersip_dialog:dialog()</a>, usages = [<a href="ersip_dialog_usage.md#type-usage">ersip_dialog_usage:usage()</a>]}
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = early | moratorium | established | mortal | morgue
</code></pre>

