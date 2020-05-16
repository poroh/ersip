

# Module ersip_route_set #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-route">route()</a> ###


<pre><code>
route() = <a href="ersip_hdr_route.md#type-route">ersip_hdr_route:route()</a>
</code></pre>




### <a name="type-route_set">route_set()</a> ###


<pre><code>
route_set() = {route_set, [<a href="#type-route">route()</a>]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_first-2">add_first/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_last-2">add_last/2</a></td><td></td></tr><tr><td valign="top"><a href="#first-1">first/1</a></td><td></td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td></td></tr><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td></td></tr><tr><td valign="top"><a href="#last-1">last/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#remove_first-1">remove_first/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove_last-1">remove_last/1</a></td><td></td></tr><tr><td valign="top"><a href="#reverse-1">reverse/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_first-2"></a>

### add_first/2 ###

<pre><code>
add_first(Route::<a href="#type-route">route()</a>, X2::<a href="#type-route_set">route_set()</a>) -&gt; <a href="#type-route_set">route_set()</a>
</code></pre>
<br />

<a name="add_last-2"></a>

### add_last/2 ###

<pre><code>
add_last(Route::<a href="#type-route">route()</a>, X2::<a href="#type-route_set">route_set()</a>) -&gt; <a href="#type-route_set">route_set()</a>
</code></pre>
<br />

<a name="first-1"></a>

### first/1 ###

<pre><code>
first(X1::<a href="#type-route_set">route_set()</a>) -&gt; <a href="#type-route">route()</a>
</code></pre>
<br />

<a name="foldl-3"></a>

### foldl/3 ###

<pre><code>
foldl(Fun, Init::any(), X3::<a href="#type-route_set">route_set()</a>) -&gt; any()
</code></pre>

<ul class="definitions"><li><code>Fun = fun((<a href="#type-route">route()</a>, any()) -&gt; any())</code></li></ul>

<a name="is_empty-1"></a>

### is_empty/1 ###

<pre><code>
is_empty(X1::<a href="#type-route_set">route_set()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="last-1"></a>

### last/1 ###

<pre><code>
last(X1::<a href="#type-route_set">route_set()</a>) -&gt; <a href="#type-route">route()</a>
</code></pre>
<br />

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-route_set">route_set()</a>
</code></pre>
<br />

<a name="remove_first-1"></a>

### remove_first/1 ###

<pre><code>
remove_first(X1::<a href="#type-route_set">route_set()</a>) -&gt; <a href="#type-route_set">route_set()</a>
</code></pre>
<br />

<a name="remove_last-1"></a>

### remove_last/1 ###

<pre><code>
remove_last(X1::<a href="#type-route_set">route_set()</a>) -&gt; <a href="#type-route_set">route_set()</a>
</code></pre>
<br />

<a name="reverse-1"></a>

### reverse/1 ###

<pre><code>
reverse(X1::<a href="#type-route_set">route_set()</a>) -&gt; <a href="#type-route_set">route_set()</a>
</code></pre>
<br />

