

# Module ersip_map #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-apply_fun">apply_fun()</a> ###


<pre><code>
apply_fun() = fun((term(), tuple()) -&gt; tuple())
</code></pre>




### <a name="type-apply_pair">apply_pair()</a> ###


<pre><code>
apply_pair() = {Key::atom(), <a href="#type-apply_fun">apply_fun()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#apply_to-3">apply_to/3</a></td><td>Apply functions to target if key is memeber of SrcMap.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="apply_to-3"></a>

### apply_to/3 ###

<pre><code>
apply_to(Appliers::[<a href="#type-apply_pair">apply_pair()</a>], SrcMap::map(), Target::any()) -&gt; any()
</code></pre>
<br />

Apply functions to target if key is memeber of SrcMap.

