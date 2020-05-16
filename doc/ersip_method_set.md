

# Module ersip_method_set #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-raw">raw()</a> ###


<pre><code>
raw() = [binary()]
</code></pre>




### <a name="type-set">set()</a> ###


<pre><code>
set() = {method_set, <a href="gb_sets.md#type-set">gb_sets:set</a>(<a href="ersip_method.md#type-method">ersip_method:method()</a>)}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#has-2">has/2</a></td><td></td></tr><tr><td valign="top"><a href="#intersection-2">intersection/2</a></td><td></td></tr><tr><td valign="top"><a href="#invite_set-0">invite_set/0</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#raw-1">raw/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="has-2"></a>

### has/2 ###

<pre><code>
has(Method::<a href="ersip_method.md#type-method">ersip_method:method()</a>, X2::<a href="#type-set">set()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="intersection-2"></a>

### intersection/2 ###

<pre><code>
intersection(X1::<a href="#type-set">set()</a>, X2::<a href="#type-set">set()</a>) -&gt; <a href="#type-set">set()</a>
</code></pre>
<br />

<a name="invite_set-0"></a>

### invite_set/0 ###

<pre><code>
invite_set() -&gt; <a href="#type-set">set()</a>
</code></pre>
<br />

<a name="make-1"></a>

### make/1 ###

<pre><code>
make(BinaryList::<a href="#type-raw">raw()</a>) -&gt; <a href="#type-set">set()</a>
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(MethodList::[<a href="ersip_method.md#type-method">ersip_method:method()</a>]) -&gt; <a href="#type-set">set()</a>
</code></pre>
<br />

<a name="raw-1"></a>

### raw/1 ###

<pre><code>
raw(X1::<a href="#type-set">set()</a>) -&gt; <a href="#type-raw">raw()</a>
</code></pre>
<br />

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(X1::<a href="#type-set">set()</a>) -&gt; [<a href="ersip_method.md#type-method">ersip_method:method()</a>]
</code></pre>
<br />

